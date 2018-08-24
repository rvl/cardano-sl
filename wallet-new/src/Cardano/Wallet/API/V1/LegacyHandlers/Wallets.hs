module Cardano.Wallet.API.V1.LegacyHandlers.Wallets (
      handlers

    -- * Internals, exposed only for testing
    , isNodeSufficientlySynced
    , newWallet
    ) where

import qualified Data.Map.Strict as Map
import           Formatting (build, sformat)
import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Pos.Chain.Update ()
import           Pos.Client.KeyStorage (addPublicKey)
import qualified Pos.Core as Core
import           Pos.Crypto (PublicKey)

import           Pos.Util (HasLens (..), maybeThrow)
import           Pos.Util.Servant (encodeCType)
import qualified Pos.Wallet.WalletMode as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic,
                     MonadWalletLogicRead)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Servant


-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: HasConfigurations
         => ServerT Wallets.API MonadV1
handlers = newWallet
    :<|> listWallets
    :<|> updatePassword
    :<|> deleteWallet
    :<|> getWallet
    :<|> updateWallet
    :<|> getUtxoStatistics
    :<|> checkExternalWallet
    :<|> newExternalWallet
    :<|> deleteExternalWallet

-- | Pure function which returns whether or not the underlying node is
-- \"synced enough\" to allow wallet creation/restoration. The notion of
-- \"synced enough\" is quite vague and if made too stringent could prevent
-- the backend to operate normally for wallets which are on a slow network
-- or are struggling to keep up. Therefore we consider a node to be \"synced
-- enough\" with the blockchain if we are not lagging more than @k@ slots, where
-- @k@ comes from the 'blkSecurityParam'.
isNodeSufficientlySynced :: Core.HasProtocolConstants => V0.SyncProgress -> Bool
isNodeSufficientlySynced spV0 =
    let blockchainHeight = fromMaybe (Core.BlockCount maxBound)
                                     (Core.getChainDifficulty <$> V0._spNetworkCD spV0)
        localHeight = Core.getChainDifficulty . V0._spLocalCD $ spV0
        remainingBlocks = blockchainHeight - localHeight

        in remainingBlocks <= Core.blkSecurityParam

-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet
    :: (MonadThrow m
       , MonadUnliftIO m
       , MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       , HasLens SyncQueue ctx SyncQueue
       )
    => NewWallet
    -> m (WalletResponse Wallet)
newWallet NewWallet{..} = do

    spV0 <- V0.syncProgress
    syncPercentage <- migrate spV0

    -- Do not allow creation or restoration of wallets if the underlying node
    -- is still catching up.
    unless (isNodeSufficientlySynced spV0) $ throwM (NodeIsStillSyncing syncPercentage)

    let newWalletHandler CreateWallet  = V0.newWalletNoThrow
        newWalletHandler RestoreWallet = V0.restoreWalletFromSeedNoThrow
        (V1 spendingPassword) = fromMaybe (V1 mempty) newwalSpendingPassword
        (BackupPhrase backupPhrase) = newwalBackupPhrase
    initMeta <- V0.CWalletMeta <$> pure newwalName
                              <*> migrate newwalAssuranceLevel
                              <*> pure 0
    let walletInit = V0.CWalletInit initMeta (V0.CBackupPhrase backupPhrase)
    single <$> do
        ev0wallet <- newWalletHandler newwalOperation spendingPassword walletInit
        case ev0wallet of
            Left cidWal -> do
                walletId <- migrate cidWal
                throwM (WalletAlreadyExists walletId)
            Right v0wallet -> do
                ss <- V0.askWalletSnapshot
                addWalletInfo ss v0wallet

-- | Returns the full (paginated) list of wallets.
listWallets :: ( MonadThrow m
               , V0.MonadWalletLogicRead ctx m
               , V0.MonadBlockchainInfo m
               )
            => RequestParams
            -> FilterOperations '[WalletId, Core.Coin] Wallet
            -> SortOperations Wallet
            -> m (WalletResponse [Wallet])
listWallets params fops sops = do
    ws <- V0.askWalletSnapshot
    currentDepth <- V0.networkChainDifficulty
    respondWith params fops sops (IxSet.fromList <$> do
        (V0.getWalletsWithInfo ws >>= (migrate @_ @[V1.Wallet] . map (\(w, i) -> (w,i,currentDepth)))))

updatePassword
    :: ( MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletId -> PasswordUpdate -> m (WalletResponse Wallet)
updatePassword wid PasswordUpdate{..} = do
    wid' <- migrate wid
    let (V1 old) = pwdOld
        (V1 new) = pwdNew
    _ <- V0.changeWalletPassphrase wid' old new
    single <$> do
        ss <- V0.askWalletSnapshot
        wallet <- V0.getWallet wid'
        addWalletInfo ss wallet

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet = V0.deleteWallet <=< migrate

getWallet :: ( MonadThrow m
             , MonadWalletLogicRead ctx m
             , V0.MonadBlockchainInfo m
             ) => WalletId -> m (WalletResponse Wallet)
getWallet wid = do
    ss <- V0.askWalletSnapshot
    wid' <- migrate wid
    wallet <- V0.getWallet wid'
    single <$> addWalletInfo ss wallet

addWalletInfo
    :: ( MonadThrow m
       , V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => V0.WalletSnapshot
    -> V0.CWallet
    -> m Wallet
addWalletInfo snapshot wallet = do
    case V0.getWalletInfo (V0.cwId wallet) snapshot of
        Nothing ->
            throwM WalletNotFound
        Just walletInfo -> do
            currentDepth <- V0.networkChainDifficulty
            migrate (wallet, walletInfo, currentDepth)

updateWallet
    :: (V0.MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletId
    -> WalletUpdate
    -> m (WalletResponse Wallet)
updateWallet wid WalletUpdate{..} = do
    ws <- V0.askWalletSnapshot
    wid' <- migrate wid
    assurance <- migrate uwalAssuranceLevel
    walletMeta <- maybe (throwM WalletNotFound) pure $ V0.getWalletMeta wid' ws
    updated <- V0.updateWallet wid' walletMeta
        { V0.cwName = uwalName
        , V0.cwAssurance = assurance
        }
    single <$> do
        -- reacquire the snapshot because we did an update
        ws' <- V0.askWalletSnapshot
        addWalletInfo ws' updated

-- | Gets Utxo statistics for a wallet.
-- | Stub, not calling data layer.
getUtxoStatistics
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m (WalletResponse UtxoStatistics)
getUtxoStatistics _ = do
    return $ single (V1.computeUtxoStatistics V1.log10 [])

-- | Check if external wallet is presented in node's wallet db.
checkExternalWallet
    :: ( V0.MonadWalletLogic ctx m
       , V0.MonadWalletHistory ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => PublicKeyAsBase58
    -> m (WalletResponse WalletAndTxHistory)
checkExternalWallet encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    ws <- V0.askWalletSnapshot
    let walletId = encodeCType . Core.makePubKeyAddressBoot $ rootPK
    walletExists <- V0.doesWalletExist walletId
    (v0wallet, transactions, isWalletReady) <- if walletExists
        then do
            -- Wallet is here, it means that user already used this wallet (for example,
            -- hardware device) on this computer, so we have to return stored information
            -- about this wallet and history of transactions (if any transactions was made).
            --
            -- By default we have to specify account and address for getting transactions
            -- history. But currently all we have is root PK, so we return complete history
            -- of transactions, for all accounts and addresses.
            let allAccounts = getWalletAccountIds ws walletId
                -- We want to get a complete history, so we shouldn't specify an address.
                address = Nothing
            (V0.WalletHistory history, _) <- V0.getHistory walletId
                                                           (const allAccounts)
                                                           address
            v1Transactions <- mapM (\(_, (v0Tx, _)) -> migrate v0Tx) $ Map.toList history
            (,,) <$> V0.getWallet walletId
                 <*> pure v1Transactions
                 <*> pure True
        else do
            -- No such wallet in db, it means that this wallet (for example, hardware
            -- device) was not used on this computer. But since this wallet _could_ be
            -- used on another computer, we have to (try to) restore this wallet.
            -- Since there's no wallet meta-data, we use default one.
            let largeCurrencyUnit = 0
                defaultMeta = V0.CWalletMeta "External wallet"
                                             V0.CWAStrict
                                             largeCurrencyUnit
                -- This is a new wallet, currently un-synchronized, so there's no
                -- history of transactions yet.
                transactions = []
            (,,) <$> restoreExternalWallet defaultMeta encodedRootPK
                 <*> pure transactions
                 <*> pure False -- We restore wallet, so it's unready yet.

    v1wallet <- migrateWallet ws v0wallet isWalletReady
    let walletAndTxs = WalletAndTxHistory v1wallet transactions
    single <$> pure walletAndTxs

-- | Creates a new or restores an existing external @wallet@ given a 'NewExternalWallet' payload.
-- Returns to the client the representation of the created or restored wallet in the 'Wallet' type.
newExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       , V0.MonadBlockchainInfo m
       , V0.MonadWalletLogic ctx m
       )
    => NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet (NewExternalWallet rootPK assuranceLevel name operation) = do
    let newExternalWalletHandler CreateWallet  = createNewExternalWallet
        newExternalWalletHandler RestoreWallet = restoreExternalWallet
    walletMeta <- V0.CWalletMeta <$> pure name
                                 <*> migrate assuranceLevel
                                 <*> pure 0
    single <$> do
        v0wallet <- newExternalWalletHandler operation walletMeta rootPK
        ws <- V0.askWalletSnapshot
        migrateWallet ws v0wallet True

-- | Creates new external wallet.
createNewExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
createNewExternalWallet walletMeta encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    -- This extended public key will be used during synchronization
    -- with the blockchain.
    addPublicKey rootPK

    let walletId = encodeCType . Core.makePubKeyAddressBoot $ rootPK
        isReady  = True -- We don't need to sync new wallet with the blockchain.

    -- Create new external wallet.
    -- This is safe: if the client will try to create an external wallet from the same
    -- root public key - error will be thrown.
    void $ V0.createWalletSafe walletId walletMeta isReady

    addInitAccountInExternalWallet walletId

    V0.getWallet walletId

-- | Restore external wallet using it's root public key and metadata.
restoreExternalWallet
    :: ( MonadThrow m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       , V0.MonadWalletLogic ctx m
       )
    => V0.CWalletMeta
    -> PublicKeyAsBase58
    -> m V0.CWallet
restoreExternalWallet walletMeta encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK

    let walletId = encodeCType . Core.makePubKeyAddressBoot $ rootPK

    -- Public key will be used during synchronization with the blockchain.
    addPublicKey rootPK

    let isReady = False -- Because we want to sync this wallet with the blockchain!

    -- Create new external wallet with initial account.
    void $ V0.createWalletSafe walletId walletMeta isReady
    addInitAccountInExternalWallet walletId

    -- Restoring this wallet.
    V0.restoreExternalWallet rootPK

addInitAccountInExternalWallet
    :: ( MonadThrow m
       , V0.MonadWalletLogic ctx m
       )
    => V0.CId V0.Wal
    -> m ()
addInitAccountInExternalWallet walletId = do
    let accountName = "Initial account"
        accountMeta = V0.CAccountMeta accountName
        accountInit = V0.CAccountInit accountMeta walletId
        includeUnready = True
    void $ V0.newExternalAccountIncludeUnready includeUnready accountInit

-- | On the disk, once imported or created, there's so far not much difference
-- between a wallet and an external wallet, except one: node stores a public key
-- for external wallet, there's no secret key.
deleteExternalWallet
    :: (V0.MonadWalletLogic ctx m)
    => PublicKeyAsBase58
    -> m NoContent
deleteExternalWallet encodedRootPK = do
    rootPK <- mkPublicKeyOrFail encodedRootPK
    V0.deleteExternalWallet rootPK

migrateWallet
    :: ( V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => V0.WalletSnapshot
    -> V0.CWallet
    -> Bool
    -> m Wallet
migrateWallet snapshot wallet walletIsReady = do
    let walletId = V0.cwId wallet
    walletInfo <- if walletIsReady
        then maybeThrow WalletNotFound $ V0.getWalletInfo walletId snapshot
        else
            -- Wallet is not ready yet (because of restoring),
            -- the only information we can provide is the default one.
            pure $ V0.getUnreadyWalletInfo snapshot
    walletIsExternal <- V0.isWalletExternal walletId
    let walletType = if walletIsExternal then WalletExternal else WalletRegular
    currentDepth <- V0.networkChainDifficulty
    migrate (wallet, walletInfo, walletType, currentDepth)

mkPublicKeyOrFail
    :: MonadThrow m
    => PublicKeyAsBase58
    -> m PublicKey
mkPublicKeyOrFail encodedRootPK =
    case mkPublicKeyFromBase58 encodedRootPK of
        Left problem -> throwM (InvalidPublicKey $ sformat build problem)
        Right rootPK -> return rootPK
