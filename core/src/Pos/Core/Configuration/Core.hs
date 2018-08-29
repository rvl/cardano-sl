{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE Rank2Types  #-}

-- | Global constants, configurable via Data.Reflection.

module Pos.Core.Configuration.Core
       (
       -- * The configuration structure
         CoreConfiguration(..)
       , GenesisConfiguration(..)

       , HasCoreConfiguration
       , withCoreConfiguration

       , coreConfiguration
       , dbSerializeVersion
       ) where

import           Prelude
import           Universum hiding (fail, (<>))

import           Data.Aeson (FromJSON, ToJSON, Value (..), genericToEncoding,
                     pairs, parseJSON, toEncoding, (.:))
import           Data.Aeson.Encoding (pairStr)
import           Data.Aeson.Options (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.Reflection (Given (..), give)

import           Pos.Binary.Class (Raw)
import           Pos.Core.Genesis (GenesisAvvmBalances (..),
                     GenesisInitializer (..), GenesisSpec (..))
import           Pos.Crypto.Hashing (Hash)

data GenesisConfiguration
      -- | Genesis from a 'GenesisSpec'.
    = GCSpec !GenesisSpec
      -- | 'GenesisData' is stored in a file.
    | GCSrc !FilePath !(Hash Raw)
      -- !FilePath = Path to file where 'GenesisData' is stored. Must be
      -- in JSON, not necessary canonical.
      -- !(Hash Raw) = Hash of canonically encoded 'GenesisData'.
    deriving (Eq, Show, Generic)

instance ToJSON GenesisConfiguration where
    toEncoding (GCSrc gcsFile gcsHash) =
        pairs . pairStr "src"
            . pairs  $ pairStr "file"
                (toEncoding gcsFile) <> pairStr "hash" (toEncoding gcsHash)

    toEncoding (GCSpec value)          =
        genericToEncoding defaultOptions (GCSpec value)

instance FromJSON GenesisConfiguration where
    parseJSON (Object o)
        | HM.member "src" o  = GCSrc <$> ((o .: "src") >>= (.: "file"))
                                      <*> ((o .: "src") >>= (.: "hash"))
        | HM.member "spec" o = do
              -- GCSpec Object
              specO <- o .: "spec"

              -- GenesisAvvmBalances
              avvmDistrO <- specO .: "avvmDistr"
              avvmDistr <- parseJSON (avvmDistrO)

              -- SharedSeed
              ftsSeed <- specO .: "ftsSeed"

              -- GenesisDelegation
              heavyDelegationO <- specO .: "heavyDelegation"
              heavyDelegation <- parseJSON (heavyDelegationO)

              -- BlockVersionData
              blockVersionDataO <- specO .: "blockVersionData"
              blockVersionData <- parseJSON blockVersionDataO

              -- GenesisProtocolConstants
              protocolConstantsO <- specO .: "protocolConstants"
              protocolConstants <- parseJSON protocolConstantsO

              -- GenesisInitializer
              initializerO <- specO .: "initializer"
              testBalanceO <- initializerO .: "testBalance"
              testBalance <- parseJSON testBalanceO
              fakeAvvmBalanceO <- (initializerO .: "fakeAvvmBalance")
              fakeAvvmBalance <- parseJSON fakeAvvmBalanceO
              avvmBalanceFactor <- initializerO .: "avvmBalanceFactor"
              useHeavyDlg <- initializerO .: "useHeavyDlg"
              seed <- initializerO .: "seed"

              return . GCSpec $
                  UnsafeGenesisSpec
                      (GenesisAvvmBalances avvmDistr)
                      ftsSeed
                      heavyDelegation
                      blockVersionData
                      protocolConstants
                      (GenesisInitializer
                          testBalance
                          fakeAvvmBalance
                          avvmBalanceFactor
                          useHeavyDlg
                          seed)
        | otherwise = fail "Incorrect JSON encoding for GenesisConfiguration"

    parseJSON invalid = typeMismatch "GenesisConfiguration" invalid

data CoreConfiguration = CoreConfiguration
    {
      -- | Specifies the genesis
      ccGenesis            :: !GenesisConfiguration

      -- | Versioning for values in node's DB
    , ccDbSerializeVersion :: !Word8

    }
    deriving (Show, Generic)

deriveJSON defaultOptions ''CoreConfiguration

type HasCoreConfiguration = Given CoreConfiguration

withCoreConfiguration :: CoreConfiguration -> (HasCoreConfiguration => r) -> r
withCoreConfiguration = give

coreConfiguration :: HasCoreConfiguration => CoreConfiguration
coreConfiguration = given

-- | DB format version. When serializing items into the node's DB, the values are paired
-- with this constant.
dbSerializeVersion :: HasCoreConfiguration => Word8
dbSerializeVersion = fromIntegral . ccDbSerializeVersion $ coreConfiguration
