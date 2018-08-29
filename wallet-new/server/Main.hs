{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Maybe (fromJust)
import           Ntp.Client (NtpConfiguration, NtpStatus, ntpClientSettings,
                     withNtpClient)
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (ncUserSecret)
import           Pos.Core as Core (Config (..), configBlkSecurityParam)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (NodeParams (..), NodeResources (..),
                     bpLoggingParams, bracketNodeResources, loggerBracket,
                     lpDefaultName, runNode, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..),
                     ConfigurationOptions, HasConfigurations)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Util.Wlog (LoggerName, Severity (..), logInfo, logMessage,
                     usingLoggerName)
import           Pos.Wallet.Web (bracketWalletWS, bracketWalletWebDB,
                     getKeyById, getWalletAddresses, runWRealMode)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (askWalletDB, askWalletSnapshot,
                     flushWalletStorage)
import           Pos.Wallet.Web.Tracking.Decrypt (keyToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (syncWallet)

import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode

import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as NodeStateAdaptor
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     NewWalletBackendParams (..), WalletBackendParams (..),
                     WalletStartupOptions (..), getWalletDbOptions,
                     getWalletNodeOptions, walletDbPath, walletFlushDb,
                     walletRebuildDb)
import qualified Cardano.Wallet.Server.Plugins as Plugins
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel

-- | Default logger name when one is not provided on the command line
defaultLoggerName :: LoggerName
defaultLoggerName = "node"

-- | The legacy function responsible for starting a Cardano edge node plus a
-- number of extra plugins.
actionWithLegacyWallet :: (HasConfigurations, HasCompileInfo)
                 => Core.Config
                 -> TxpConfiguration
                 -> SscParams
                 -> NodeParams
                 -> NtpConfiguration
                 -> WalletBackendParams
                 -> IO ()
actionWithLegacyWallet coreConfig txpConfig sscParams nodeParams ntpConfig wArgs@WalletBackendParams {..} =
    bracketWalletWebDB (walletDbPath walletDbOptions) (walletRebuildDb walletDbOptions) $ \db ->
        bracketWalletWS $ \conn ->
            bracketNodeResources
                coreConfig
                nodeParams
                sscParams
                (txpGlobalSettings coreConfig txpConfig)
                (initNodeDBs coreConfig) $ \nr@NodeResources {..} -> do
                    syncQueue <- liftIO newTQueueIO
                    ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
                    runWRealMode coreConfig txpConfig db conn syncQueue nr (mainAction ntpStatus nr)
  where
    mainAction ntpStatus = runNodeWithInit ntpStatus $ do
        when (walletFlushDb walletDbOptions) $ do
            logInfo "Flushing wallet db..."
            askWalletDB >>= flushWalletStorage
            logInfo "Resyncing wallets with blockchain..."

        -- NOTE(adn): Sync the wallets anyway. The old implementation was skipping syncing in
        -- case `walletFlushDb` was not set, but was still calling it before starting the Servant
        -- server.
        syncWallets

    runNodeWithInit ntpStatus init' nr diffusion = do
        _ <- init'
        runNode coreConfig txpConfig nr (plugins ntpStatus) diffusion

    syncWallets :: WalletWebMode ()
    syncWallets = do
        addrs <- getWalletAddresses <$> askWalletSnapshot
        keys' <- mapM getKeyById addrs
        forM_ keys' (syncWallet . keyToWalletDecrCredentials)

    plugins :: TVar NtpStatus -> Plugins.Plugin WalletWebMode
    plugins ntpStatus =
        mconcat [ Plugins.conversation wArgs
                , Plugins.legacyWalletBackend coreConfig txpConfig wArgs ntpStatus
                , Plugins.walletDocumentation wArgs
                , Plugins.acidCleanupWorker wArgs
                , Plugins.syncWalletWorker (configBlkSecurityParam coreConfig)
                , Plugins.resubmitterPlugin coreConfig txpConfig
                , Plugins.notifierPlugin
                ]

-- | The "workhorse" responsible for starting a Cardano edge node plus a number of extra plugins.
actionWithWallet :: (HasConfigurations, HasCompileInfo)
                 => Core.Config
                 -> TxpConfiguration
                 -> SscParams
                 -> NodeParams
                 -> NtpConfiguration
                 -> NewWalletBackendParams
                 -> IO ()
actionWithWallet coreConfig txpConfig sscParams nodeParams ntpConfig params =
    bracketNodeResources
        coreConfig
        nodeParams
        sscParams
        (txpGlobalSettings coreConfig txpConfig)
        (initNodeDBs coreConfig) $ \nr -> do
      ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
      userSecret <- readTVarIO (ncUserSecret $ nrContext nr)
      let nodeState = NodeStateAdaptor.newNodeStateAdaptor
              coreConfig
              nr
              ntpStatus
      liftIO $ Keystore.bracketLegacyKeystore userSecret $ \keystore -> do
          let dbPath = walletDbPath (getWalletDbOptions params)
          let dbMode = Kernel.UseFilePath (Kernel.DatabasePaths {
                Kernel.dbPathAcidState = Just (dbPath <> "-acid")
              , Kernel.dbPathMetadata  = Just (dbPath <> "-sqlite.sqlite3")
              })
          WalletLayer.Kernel.bracketPassiveWallet dbMode logMessage' keystore nodeState $ \walletLayer passiveWallet -> do
              Kernel.init passiveWallet
              Kernel.Mode.runWalletMode coreConfig
                                        txpConfig
                                        nr
                                        walletLayer
                                        (mainAction (walletLayer, passiveWallet) nr)
  where
    pm = configProtocolMagic coreConfig
    mainAction
        :: (PassiveWalletLayer IO, PassiveWallet)
        -> NodeResources ext
        -> (Diffusion Kernel.Mode.WalletMode -> Kernel.Mode.WalletMode ())
    mainAction w nr = runNodeWithInit w nr

    runNodeWithInit
        :: (PassiveWalletLayer IO, PassiveWallet)
        -> NodeResources ext
        -> (Diffusion Kernel.Mode.WalletMode -> Kernel.Mode.WalletMode ())
    runNodeWithInit w nr = runNode coreConfig txpConfig nr (plugins w)

    -- TODO: Don't know if we need any of the other plugins that are used
    -- in the legacy wallet (see 'actionWithLegacyWallet').
    plugins :: (PassiveWalletLayer IO, PassiveWallet)
            -> Plugins.Plugin Kernel.Mode.WalletMode
    plugins w = mconcat [ Plugins.walletBackend pm params w ]

    -- Extract the logger name from node parameters
    --
    -- TODO: Not sure what the policy is for logger names of components.
    -- For now we just use the one from the node itself.
    logMessage' :: Severity -> Text -> IO ()
    logMessage' sev txt =
        usingLoggerName loggerName $ logMessage sev txt
      where
        loggerName :: LoggerName
        loggerName = lpDefaultName . bpLoggingParams . npBaseParams $ nodeParams

-- | Runs an edge node plus its wallet backend API.
startEdgeNode :: HasCompileInfo => WalletStartupOptions -> IO ()
startEdgeNode wso =
    withConfigurations blPath conf $ \coreConfig txpConfig ntpConfig -> do
        (sscParams, nodeParams) <- getParameters coreConfig txpConfig ntpConfig
        case wsoWalletBackendParams wso of
            WalletLegacy legacyParams -> actionWithLegacyWallet
                coreConfig
                txpConfig
                sscParams
                nodeParams
                ntpConfig
                legacyParams
            WalletNew newParams -> actionWithWallet
                coreConfig
                txpConfig
                sscParams
                nodeParams
                ntpConfig
                newParams
  where
    getParameters :: HasConfigurations
                  => Core.Config
                  -> TxpConfiguration
                  -> NtpConfiguration
                  -> IO (SscParams, NodeParams)
    getParameters coreConfig txpConfig ntpConfig = do

      currentParams <- CLI.getNodeParams defaultLoggerName
                                         (wsoNodeArgs wso)
                                         nodeArgs
                                         (configGeneratedSecrets coreConfig)
      let vssSK = fromJust $ npUserSecret currentParams ^. usVss
      let gtParams = CLI.gtSscParams (wsoNodeArgs wso) vssSK (npBehaviorConfig currentParams)

      CLI.printInfoOnStart (wsoNodeArgs wso)
                           (configGenesisData coreConfig)
                           ntpConfig
                           txpConfig
      logInfo "Wallet is enabled!"

      return (gtParams, currentParams)

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs (wsoNodeArgs wso)

    blPath :: Maybe AssetLockPath
    blPath = AssetLockPath <$> CLI.cnaAssetLockPath (wsoNodeArgs wso)

    nodeArgs :: CLI.NodeArgs
    nodeArgs = CLI.NodeArgs { CLI.behaviorConfigPath = Nothing }


-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    cfg <- getWalletNodeOptions
    putText "Wallet is starting..."
    let loggingParams = CLI.loggingParams defaultLoggerName (wsoNodeArgs cfg)
    loggerBracket loggingParams . logException "node" $ do
        logInfo "[Attention] Software is built with the wallet backend"
        startEdgeNode cfg
