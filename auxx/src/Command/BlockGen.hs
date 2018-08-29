{-# LANGUAGE RecordWildCards #-}

-- | Block generation.

module Command.BlockGen
       ( generateBlocks
       ) where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default (def)
import           System.Random (mkStdGen, randomIO)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Core as Core (Config (..), configBootStakeholders)
import           Pos.Crypto (encToSecret)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Generator.Block (BlockGenParams (..), genBlocks,
                     tgpTxCountRange)
import           Pos.Infra.StateLock (Priority (..), withStateLock)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason (..))
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Wlog (logInfo)

import           Lang.Value (GenBlocksParams (..))
import           Mode (MonadAuxxMode)

generateBlocks :: MonadAuxxMode m
               => Core.Config
               -> TxpConfiguration
               -> GenBlocksParams
               -> m ()
generateBlocks coreConfig txpConfig GenBlocksParams{..} = withStateLock HighPriority ApplyBlock $ \_ -> do
    seed <- liftIO $ maybe randomIO pure bgoSeed
    logInfo $ "Generating with seed " <> show seed

    allSecrets <- mkAllSecretsSimple . map encToSecret <$> getSecretKeysPlain

    let bgenParams =
            BlockGenParams
                { _bgpSecrets         = allSecrets
                , _bgpGenStakeholders = configBootStakeholders coreConfig
                , _bgpBlockCount      = fromIntegral bgoBlockN
                -- tx generation is disabled for now
                , _bgpTxGenParams     = def & tgpTxCountRange .~ (0,0)
                , _bgpInplaceDB       = True
                , _bgpSkipNoKey       = True
                , _bgpTxpGlobalSettings = txpGlobalSettings coreConfig txpConfig
                }
    withCompileInfo $ evalRandT
        (genBlocks coreConfig txpConfig bgenParams (const ()))
        (mkStdGen seed)
    -- We print it twice because there can be a ton of logs and
    -- you don't notice the first message.
    logInfo $ "Generated with seed " <> show seed
