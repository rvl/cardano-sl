{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes     #-}
-- | Wallet state as mandated by the wallet specification
module Cardano.Wallet.Kernel.DB.Spec (
    -- * Checkpoint
    Checkpoint(..)
  , Checkpoints(..)
  , initCheckpoint
  , liftCheckpoints
    -- ** Lenses
  , checkpointUtxo
  , checkpointUtxoBalance
  , checkpointPending
  , checkpointBlockMeta
  , checkpointForeign
  , checkpointContext
    -- * Partial checkpoints
  , PartialCheckpoint(..)
  , initPartialCheckpoint
  , fromFullCheckpoint
  , toFullCheckpoint
    -- ** Lenses
  , unCheckpoints
  , pcheckpointUtxo
  , pcheckpointUtxoBalance
  , pcheckpointPending
  , pcheckpointBlockMeta
  , pcheckpointForeign
  , pcheckpointContext
    -- * Unify partial and full checkpoints
  , IsCheckpoint(..)
  , cpAddressMeta
    -- ** Convenience: lift lenses on single checkpoint to the most recent one
  , currentCheckpoint
  , currentUtxo
  , currentUtxoBalance
  , currentPending
  , currentBlockMeta
  , currentContext
  , currentAddressMeta
  , currentForeign
    -- ** Convenience: accessors for other checkpoints
  , oldestCheckpoint
    -- ** public for testing compression
  , deltas
  , steps
  , deltaC
  , stepC
  , deltaPartialC
  , stepPartialC
  ) where

import           Universum

import           Control.Lens (Getter, from, to, _Wrapped)
import           Control.Lens.TH (makeLenses)
import           Data.Coerce (coerce)
import qualified Data.SafeCopy as SC
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util.Text (mapJson)
import           Test.QuickCheck (Arbitrary (..))

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import           Pos.Core.Chrono (NewestFirst (..))

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.Compression
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.Util.Core as Core
import qualified Cardano.Wallet.Kernel.Util.StrictList as SL
import           Cardano.Wallet.Kernel.Util.StrictNonEmpty (StrictNonEmpty (..))
import qualified Cardano.Wallet.Kernel.Util.StrictNonEmpty as SNE

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Core.Arbitrary.Txp ()

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

{-------------------------------------------------------------------------------
  Wallet state as mandated by the spec
-------------------------------------------------------------------------------}

-- | Per-wallet state
--
-- NOTE: At the moment this does not included the expected UTxO. The expected
-- UTxO is used for two things:
--
-- * Block resolution (translating tx inputs to their corresponding outputs, so
--   that we know the corresponding addresses, needed for prefilering)
-- * Minimum balance computation
--
-- Fortunately however we can rely on a full node as backing, so we don't need
-- to use the expected UTxO for block resolution (this is explained in the
-- formal spec in section "Prefiltering -- Consequences", under "possible
-- alternatives"), and minimum balance computation is a new feature that we
-- haven't implemented yet.
--
-- NOTE: This is the same across all wallet types.
data Checkpoint = Checkpoint {
      _checkpointUtxo        :: !(InDb Core.Utxo)
    , _checkpointUtxoBalance :: !(InDb Core.Coin)
    , _checkpointPending     :: !Pending
    , _checkpointBlockMeta   :: !BlockMeta

      -- Foreign pending transactions are transactions that transfer funds from
      -- /other/ wallets /to/ this wallet. An example are redemption
      -- certificates, which (logically) transfer money from an "AVVM wallet" to
      -- this one; crucially, this wallet would not recognize the input of a
      -- redemption transaction as " ours ".
    , _checkpointForeign     :: !Pending

      -- | Block context of this block
      --
      -- Set to 'Nothing' for the initial checkpoint only.
      --
      -- The block context is used for a number of purposes:
      --
      -- * During restoration we use it to check whether or not we have
      --   bridged the gap between the current and historical checkpoints,
      --   as well as for reporting progress.
      -- * When applying a block, it is used to determine whether the wallet
      --   behind have fallen behind the node. (This will happen only under
      --   exceptional circumstances: for example, when the node informs the
      --   wallet of a new block, but the wallet crashes or is terminated before
      --   it can process the block.)
    , _checkpointContext     :: !(Maybe BlockContext)
    } deriving (Eq, Show)

makeLenses ''Checkpoint

SC.deriveSafeCopy 1 'SC.base ''Checkpoint

-- | Initial checkpoint for an account
--
-- This takes a UTxO as argument to allow for wallets that are given an initial
-- UTxO in the genesis block (note that we never roll back past the initial
-- checkpoint).
initCheckpoint :: Core.Utxo -> Checkpoint
initCheckpoint utxo = Checkpoint {
      _checkpointUtxo        = InDb utxo
    , _checkpointUtxoBalance = InDb $ Core.unsafeIntegerToCoin $
                                        Core.utxoBalance utxo
    , _checkpointPending     = Pending.empty
    , _checkpointForeign     = Pending.empty
    , _checkpointBlockMeta   = emptyBlockMeta
    , _checkpointContext     = Nothing
    }

{-------------------------------------------------------------------------------
  Partial checkpoints
-------------------------------------------------------------------------------}

-- | Partial checkpoint
--
-- Partial checkpoints arise during wallet restoration. They differ from
-- regular checkpoints only in that we lack historical context and therefore
-- cannot construct complete block metadata. Instead, we must approximate the
-- block metadata using local information only. The intention is that if there
-- are multiple partial checkpoints, we accumulate local block metadata so that
-- the most accurate information possible.
data PartialCheckpoint = PartialCheckpoint {
      _pcheckpointUtxo        :: !(InDb Core.Utxo)
    , _pcheckpointUtxoBalance :: !(InDb Core.Coin)
    , _pcheckpointPending     :: !Pending
    , _pcheckpointBlockMeta   :: !LocalBlockMeta
    , _pcheckpointForeign     :: !Pending
    , _pcheckpointContext     :: !(Maybe BlockContext)
    } deriving (Eq, Show)

makeLenses ''PartialCheckpoint

SC.deriveSafeCopy 1 'SC.base ''PartialCheckpoint

-- | Initial partial checkpoint when we are restoring a wallet
--
-- NOTE: The UTxO for the partial checkpoint must be obtained by looking at the
-- UTxO of the underlying full node. HOWEVER, we do not have access to the
-- block metadata for the most recent block! We have (partial) block metadata
-- for all blocks /after/ the initial partial checkpoint, and we have (complete)
-- block metadata for all historical checkpoints that we recover, but this is
-- only checkpoint for which we have no block metadata at all. Therefore we set
-- the block metadata to 'emptyBlockMeta'. Then during restoration when we are
-- recovering  historical checkpoints, we don't stop until the historical
-- checkpoints /overlap/ one block with the partial checkpoints, so that the
-- block metadata of this initial partial checkpoint is not used.
--
-- See also 'finishRestoration'.
initPartialCheckpoint :: Core.Utxo -> PartialCheckpoint
initPartialCheckpoint utxo = PartialCheckpoint {
      _pcheckpointUtxo        = InDb $ utxo
    , _pcheckpointUtxoBalance = InDb $ Core.unsafeIntegerToCoin $
                                         Core.utxoBalance utxo
    , _pcheckpointPending     = Pending.empty
    , _pcheckpointForeign     = Pending.empty
    , _pcheckpointBlockMeta   = LocalBlockMeta emptyBlockMeta
    , _pcheckpointContext     = Nothing
    }

-- | A full check point can be " downcast " to a partial checkpoint by
-- forgetting that we have complete block metadata.
--
-- We /could/ define this as an 'Iso', but prefer to define it as a 'Lens'
-- to emphasize that a partial checkpoint records less information than a
-- full checkpoint.
fromFullCheckpoint :: Lens' Checkpoint PartialCheckpoint
fromFullCheckpoint f cp = inj <$> f (proj cp)
  where
    proj :: Checkpoint -> PartialCheckpoint
    proj Checkpoint{..} = PartialCheckpoint {
          _pcheckpointUtxo        =        _checkpointUtxo
        , _pcheckpointUtxoBalance =        _checkpointUtxoBalance
        , _pcheckpointPending     =        _checkpointPending
        , _pcheckpointBlockMeta   = coerce _checkpointBlockMeta
        , _pcheckpointForeign     =        _checkpointForeign
        , _pcheckpointContext     =        _checkpointContext
        }

    inj :: PartialCheckpoint -> Checkpoint
    inj PartialCheckpoint{..} = Checkpoint{
          _checkpointUtxo        =        _pcheckpointUtxo
        , _checkpointUtxoBalance =        _pcheckpointUtxoBalance
        , _checkpointPending     =        _pcheckpointPending
        , _checkpointBlockMeta   = coerce _pcheckpointBlockMeta
        , _checkpointForeign     =        _pcheckpointForeign
        , _checkpointContext     =        _pcheckpointContext
        }

-- | Construct a full checkpoint from a partial checkpoint and the block meta
-- of chain before the first partial checkpoint.
toFullCheckpoint :: BlockMeta -> PartialCheckpoint -> Checkpoint
toFullCheckpoint prevBlockMeta PartialCheckpoint{..} = Checkpoint {
      _checkpointUtxo        =          _pcheckpointUtxo
    , _checkpointUtxoBalance =          _pcheckpointUtxoBalance
    , _checkpointPending     =          _pcheckpointPending
    , _checkpointBlockMeta   = withPrev _pcheckpointBlockMeta
    , _checkpointContext     =          _pcheckpointContext
    , _checkpointForeign     =          _pcheckpointForeign
    }
  where
    withPrev :: LocalBlockMeta -> BlockMeta
    withPrev = appendBlockMeta prevBlockMeta

{-------------------------------------------------------------------------------
  Checkpoints Wrapper
-------------------------------------------------------------------------------}

newtype Checkpoints c = Checkpoints {_unCheckpoints :: NewestFirst StrictNonEmpty c}
    deriving (Eq, Foldable, Show)

makeLenses ''Checkpoints

liftCheckpoints :: (NewestFirst StrictNonEmpty c1 -> NewestFirst StrictNonEmpty c2)
                -> Checkpoints c1 -> Checkpoints c2
liftCheckpoints f (Checkpoints cs) = Checkpoints (f cs)

deltas :: IsCheckpoint c =>  Checkpoints c -> (c, [DeltaCheckpoint])
deltas (Checkpoints (NewestFirst (a SNE.:| strict))) = (a, go a strict)
  where
    go :: IsCheckpoint c => c -> SL.StrictList c -> [DeltaCheckpoint]
    go _ SL.Nil                  = []
    go c (SL.Cons c' strictRest) = (delta c' c) : (go c' strictRest)

steps :: IsCheckpoint c => (c, [DeltaCheckpoint] ) -> Checkpoints c
steps (c, ls) = Checkpoints . NewestFirst $ c SNE.:| go c ls
  where
    go :: IsCheckpoint c => c -> [DeltaCheckpoint] -> SL.StrictList c
    go _ []         = SL.Nil
    go c' (dc:rest) = let new = step c' dc in SL.Cons new (go new rest)

instance (IsCheckpoint c, SC.SafeCopy c) => SC.SafeCopy (Checkpoints c) where
  getCopy = SC.contain $ do
    ds <- SC.safeGet
    pure $ steps ds
  putCopy cs  = SC.contain $ do
    let dcs = deltas cs
    SC.safePut dcs

{-------------------------------------------------------------------------------
  Unify over full and partial checkpoints
-------------------------------------------------------------------------------}

-- | Unify over full and partial checkpoints
--
-- Although we can treat a full checkpoint as a partial checkpoint (through
-- 'fromFullCheckpoint'), writing
--
-- > foo :: PartialCheckpoint -> ...
--
-- suggests that @foo@ should only be applied to partial checkpoints; it's
-- cleaner to write
--
-- > foo :: IsCheckpoint c => c -> ...
--
-- for functions @foo@ that can be applied to either
class IsCheckpoint c where
    cpUtxo        :: Lens' c Core.Utxo
    cpUtxoBalance :: Lens' c Core.Coin
    cpPending     :: Lens' c Pending
    cpBlockMeta   :: Lens' c LocalBlockMeta
    cpForeign     :: Lens' c Pending
    cpContext     :: Lens' c (Maybe BlockContext)
    delta         :: c -> c -> DeltaCheckpoint
    step          :: c -> DeltaCheckpoint -> c

instance IsCheckpoint Checkpoint where
    cpUtxo        = checkpointUtxo . fromDb
    cpUtxoBalance = checkpointUtxoBalance . fromDb
    cpPending     = checkpointPending
    cpBlockMeta   = checkpointBlockMeta . from _Wrapped
    cpForeign     = checkpointForeign
    cpContext     = checkpointContext
    delta         = deltaC
    step          = stepC

instance IsCheckpoint PartialCheckpoint where
    cpUtxo        = pcheckpointUtxo . fromDb
    cpUtxoBalance = pcheckpointUtxoBalance . fromDb
    cpPending     = pcheckpointPending
    cpBlockMeta   = pcheckpointBlockMeta
    cpForeign     = pcheckpointForeign
    cpContext     = pcheckpointContext
    delta         = deltaPartialC
    step          = stepPartialC

cpAddressMeta :: IsCheckpoint c => Core.Address -> Lens' c AddressMeta
cpAddressMeta addr = cpBlockMeta . _Wrapped . addressMeta addr

{-------------------------------------------------------------------------------
  Convenience: definitions for compression
-------------------------------------------------------------------------------}

deltaC :: Checkpoint -> Checkpoint -> DeltaCheckpoint
deltaC c c' = DeltaCheckpoint {
    dcUtxo         = deltaUtxo (_checkpointUtxo c) (_checkpointUtxo c')
  , dcUtxoBalance  = _checkpointUtxoBalance c
  , dcPending      = deltaPending (_checkpointPending c) (_checkpointPending c')
  , dcBlockMeta    = deltaBlockMeta (_checkpointBlockMeta c) (_checkpointBlockMeta c')
  , dcForeign      = deltaPending (_checkpointForeign c) (_checkpointForeign c')
  , dcContext      = _checkpointContext c
}

stepC :: Checkpoint -> DeltaCheckpoint -> Checkpoint
stepC c DeltaCheckpoint{..} =
  Checkpoint {
    _checkpointUtxo          = stepUtxo (_checkpointUtxo c) dcUtxo
    , _checkpointUtxoBalance = dcUtxoBalance
    , _checkpointPending     = stepPending (_checkpointPending c) dcPending
    , _checkpointBlockMeta   = stepBlockMeta (_checkpointBlockMeta c) dcBlockMeta
    , _checkpointContext     = dcContext
    , _checkpointForeign     = stepPending (_checkpointForeign c) dcForeign
  }

deltaPartialC :: PartialCheckpoint -> PartialCheckpoint -> DeltaCheckpoint
deltaPartialC c c' = DeltaCheckpoint {
    dcUtxo         = deltaUtxo (_pcheckpointUtxo c) (_pcheckpointUtxo c')
  , dcUtxoBalance  = _pcheckpointUtxoBalance c
  , dcPending      = deltaPending (_pcheckpointPending c) (_pcheckpointPending c')
  , dcBlockMeta    = deltaBlockMeta (localBlockMeta . _pcheckpointBlockMeta $ c)
                                    (localBlockMeta . _pcheckpointBlockMeta $ c')
  , dcForeign      = deltaPending (_pcheckpointForeign c) (_pcheckpointForeign c')
  , dcContext      = _pcheckpointContext c
}

stepPartialC :: PartialCheckpoint -> DeltaCheckpoint -> PartialCheckpoint
stepPartialC c DeltaCheckpoint{..} =
  PartialCheckpoint {
    _pcheckpointUtxo          = stepUtxo (_pcheckpointUtxo c) dcUtxo
    , _pcheckpointUtxoBalance = dcUtxoBalance
    , _pcheckpointPending     = stepPending (_pcheckpointPending c) dcPending
    , _pcheckpointBlockMeta   = LocalBlockMeta $ stepBlockMeta (localBlockMeta ._pcheckpointBlockMeta $ c) dcBlockMeta
    , _pcheckpointForeign     = stepPending (_pcheckpointForeign c) dcForeign
    , _pcheckpointContext     = dcContext
  }


{-------------------------------------------------------------------------------
  Convenience: lift lenses on single checkpoint to the most recent one
-------------------------------------------------------------------------------}

currentCheckpoint :: Lens' (Checkpoints c) c
currentCheckpoint = unCheckpoints . _Wrapped . SNE.head

currentUtxo        :: IsCheckpoint c => Lens' (Checkpoints c) Core.Utxo
currentUtxoBalance :: IsCheckpoint c => Lens' (Checkpoints c) Core.Coin
currentPending     :: IsCheckpoint c => Lens' (Checkpoints c) Pending
currentBlockMeta   :: IsCheckpoint c => Lens' (Checkpoints c) LocalBlockMeta
currentForeign     :: IsCheckpoint c => Lens' (Checkpoints c) Pending
currentContext     :: IsCheckpoint c => Lens' (Checkpoints c) (Maybe BlockContext)

currentUtxo        = currentCheckpoint . cpUtxo
currentUtxoBalance = currentCheckpoint . cpUtxoBalance
currentPending     = currentCheckpoint . cpPending
currentBlockMeta   = currentCheckpoint . cpBlockMeta
currentForeign     = currentCheckpoint . cpForeign
currentContext     = currentCheckpoint . cpContext

currentAddressMeta :: IsCheckpoint c => Core.Address -> Lens' (Checkpoints c) AddressMeta
currentAddressMeta addr = currentCheckpoint . cpAddressMeta addr

{-------------------------------------------------------------------------------
  Convenience: accessors for other checkpoints
-------------------------------------------------------------------------------}

oldestCheckpoint :: Getter (NewestFirst StrictNonEmpty c) c
oldestCheckpoint = _Wrapped . to SNE.last


instance Arbitrary Checkpoint where
  arbitrary = Checkpoint <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

instance Arbitrary PartialCheckpoint where
  arbitrary = PartialCheckpoint <$> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary

instance Arbitrary c => Arbitrary (Checkpoints c) where
  arbitrary = do
    ls <- arbitrary
    pure . Checkpoints . NewestFirst $ ls

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Checkpoint where
    build Checkpoint{..} = bprint
        ( "Checkpoint "
        % "{ utxo:        " % mapJson
        % ", utxoBalance: " % build
        % ", pending:     " % build
        % ", blockMeta:   " % build
        % ", context:     " % build
        % ", foreign:     " % build
        % "}"
        )
        (_fromDb _checkpointUtxo)
        (_fromDb _checkpointUtxoBalance)
        _checkpointPending
        _checkpointBlockMeta
        _checkpointContext
        _checkpointForeign

instance Buildable PartialCheckpoint where
    build PartialCheckpoint{..} = bprint
        ( "PartialCheckpoint "
        % "{ utxo:        " % mapJson
        % ", utxoBalance: " % build
        % ", pending:     " % build
        % ", blockMeta:   " % build
        % ", context:     " % build
        % ", foreign:     " % build
        % "}"
        )
        (_fromDb _pcheckpointUtxo)
        (_fromDb _pcheckpointUtxoBalance)
        _pcheckpointPending
        _pcheckpointBlockMeta
        _pcheckpointContext
        _pcheckpointForeign
