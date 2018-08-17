{-# LANGUAGE RecordWildCards #-}

module Pos.Core.Genesis.ProtocolConstants
       ( GenesisProtocolConstants (..)
       , genesisProtocolConstantsToProtocolConstants
       , genesisProtocolConstantsFromProtocolConstants
       ) where

import           Universum

import           Data.Aeson.Options (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Text.JSON.Canonical (FromJSON (..), Int54, JSValue (..),
                     ReportSchemaErrors, ToJSON (..), expected, fromJSField,
                     mkObject)

import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     VssMaxTTL (..), VssMinTTL (..))
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.Util.Json.Canonical ()
import           Pos.Util.Util (toAesonError)

-- | Bool-isomorphic flag indicating whether we're on testnet
-- or mainnet/staging.
data RequiresNetworkMagic
    = NMMustBeNothing
    | NMMustBeJust
    deriving (Show, Eq)

-- Aeson JSON instances
-- N.B @RequiresNetworkMagic@'s ToJSON & FromJSON instances do not round-trip.
-- They should only be used from a parent instance which handles the
-- `requiresNetworkMagic` key.
instance A.ToJSON RequiresNetworkMagic where
    toJSON NMMustBeNothing = A.String "NMMustBeNothing"
    toJSON NMMustBeJust    = A.String "NMMustBeJust"

instance A.FromJSON RequiresNetworkMagic where
    parseJSON = A.withText "requiresNetworkMagic" $ toAesonError . \case
        "NMMustBeNothing" -> Right NMMustBeNothing
        "NMMustBeJust"    -> Right NMMustBeJust
        other   -> Left ("invalid value " <> show other <>
                         ", acceptable values are NMMustBeNothing | NMMustBeJust")

-- Canonical JSON instances
instance Monad m => ToJSON m RequiresNetworkMagic where
    toJSON NMMustBeNothing = pure (JSString "NMMustBeNothing")
    toJSON NMMustBeJust    = pure (JSString "NMMustBeJust")

instance ReportSchemaErrors m => FromJSON m RequiresNetworkMagic where
    fromJSON = \case
        (JSString "NMMustBeNothing") -> pure NMMustBeNothing
        (JSString    "NMMustBeJust") -> pure NMMustBeJust
        other                        ->
            expected "NMMustBeNothing | NMMustBeJust" (Just (show other))

-- | 'GensisProtocolConstants' are not really part of genesis global state,
-- but they affect consensus, so they are part of 'GenesisSpec' and
-- 'GenesisData'.
data GenesisProtocolConstants = GenesisProtocolConstants
    { -- | Security parameter from the paper.
      gpcK             :: !Int
      -- | Magic constant for separating real/testnet.
    , gpcProtocolMagic :: !ProtocolMagic
      -- | VSS certificates max timeout to live (number of epochs).
    , gpcVssMaxTTL     :: !VssMaxTTL
      -- | VSS certificates min timeout to live (number of epochs).
    , gpcVssMinTTL     :: !VssMinTTL
    } deriving (Show, Eq, Generic)

instance Monad m => ToJSON m GenesisProtocolConstants where
    toJSON GenesisProtocolConstants {..} =
        mkObject
            -- 'k' definitely won't exceed the limit
            [ ("k", pure . JSNum . fromIntegral $ gpcK)
            , ("protocolMagic", toJSON gpcProtocolMagic)
            , ("vssMaxTTL", toJSON gpcVssMaxTTL)
            , ("vssMinTTL", toJSON gpcVssMinTTL)
            ]

instance ReportSchemaErrors m => FromJSON m GenesisProtocolConstants where
    fromJSON obj = do
        gpcK <- fromIntegral @Int54 <$> fromJSField obj "k"
        gpcProtocolMagic <- fromJSField obj "protocolMagic"
        gpcVssMaxTTL <- fromJSField obj "vssMaxTTL"
        gpcVssMinTTL <- fromJSField obj "vssMinTTL"
        return GenesisProtocolConstants {..}

deriveJSON defaultOptions ''GenesisProtocolConstants

genesisProtocolConstantsToProtocolConstants
    :: GenesisProtocolConstants
    -> ProtocolConstants
genesisProtocolConstantsToProtocolConstants GenesisProtocolConstants {..} =
    ProtocolConstants
        { pcK = gpcK
        , pcVssMinTTL = gpcVssMinTTL
        , pcVssMaxTTL = gpcVssMaxTTL
        }

genesisProtocolConstantsFromProtocolConstants
    :: ProtocolConstants
    -> ProtocolMagic
    -> GenesisProtocolConstants
genesisProtocolConstantsFromProtocolConstants ProtocolConstants {..} pm =
    GenesisProtocolConstants
        { gpcK = pcK
        , gpcProtocolMagic = pm
        , gpcVssMinTTL = pcVssMinTTL
        , gpcVssMaxTTL = pcVssMaxTTL
        }
