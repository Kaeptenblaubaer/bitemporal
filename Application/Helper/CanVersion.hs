module Application.Helper.CanVersion where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Exts
import GHC.Records
import GHC.Generics
import Data.Maybe
import Generated.Types
import Application.Script.Prelude
import IHP.Log as Log
import IHP.Log.Types
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import Database.PostgreSQL.Simple ( Query, ToRow )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Text.Read as T (decimal)
-- import Application.Helper.VersionTree
import Application.Helper.WorkflowProgress


today :: IO Day -- :: (year,month,day)
today = getCurrentTime >>= return . utctDay


class (Show rec, KnownSymbol (GetTableName rec), rec ~ GetModelByTableName (GetTableName rec), PrimaryKey (GetTableName rec) ~ Integer, Record rec,
    CanCreate rec, CanUpdate rec, Fetchable (QueryBuilder (GetTableName rec))  rec, FromRow rec,
    HasField "id" rec (Id rec), Show (PrimaryKey (GetTableName rec)), HasField "refHistory" rec (Id History),SetField "refHistory" rec (Id History),
    HasField "refValidfromversion" rec (Id Version), SetField "refValidfromversion" rec (Id Version),
    HasField "refValidthruversion" rec (Maybe(Id Version)), SetField "refValidthruversion" rec (Maybe (Id Version)),
    HasField "content" rec Text, SetField "content" rec Text, HasTxnLog rec) => CanVersion rec 
    where

    getKey :: rec -> Integer
    default getKey :: rec -> Integer
    getKey m = case decimal $ recordToInputValue m of
                                    Left _ -> -1
                                    Right ( i , _) -> i
    getAccessor :: (WorkflowProgress ->  Maybe (StateKeys (Id rec)))
    getWorkFlowState :: WorkflowProgress ->  Maybe (StateKeys (Id rec))
    getWorkFlowState wfp = getAccessor wfp
    setWorkFlowState :: WorkflowProgress -> Maybe (StateKeys (Id rec)) -> WorkflowProgress
    updateWfpV :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> WorkflowProgress -> UUID -> Integer -> Id rec -> Value
    updateWfpV accessor wfp h v s = fromJust $ decode $ encode $ setWorkFlowState wfp (Just ((fromJust $ accessor wfp) { history= Just h, version= Just v, state= Just s } ))
    initialWfpV:: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> UUID -> Value
    initialWfpV accessor h = fromJust $ decode $ encode $ setWorkFlowState workflowProgressDefault (Just ((fromJust $ accessor workflowProgressDefault) { history= Just h} ))
    getStatehistoryIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> WorkflowProgress -> Maybe UUID
    getStatehistoryIdMB accessor wfp = history =<< accessor wfp
    getStateVersionIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) ->WorkflowProgress -> Maybe Integer
    getStateVersionIdMB accessor wfp = version =<< accessor wfp
    getStateIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) ->WorkflowProgress -> Maybe (Id rec)
    getStateIdMB  accessor wfp = state =<< accessor wfp

    mkPersistenceLogState :: CRULog (Id rec) -> PersistenceLog

    commitState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> Workflow -> IO (Either Text Text)
    commitState accessor workflow = do
        let workflowId = get #id workflow
        Log.info $ "ToCOmmitWF wf=" ++ show workflowId
        let wfpMB = getWfp workflow
        Log.info ("ende"::String)
        putStrLn "Ende commit"
        case wfpMB of
            Just wfp -> do
                case getStatehistoryIdMB accessor wfp of
                    Just h -> case getStateVersionIdMB accessor wfp of
                        Just v -> case getStateIdMB accessor wfp of
                            Just s -> withTransaction do 
                                Log.info $ "committing h:" ++ show h
                                hUnlocked :: History <- fetch (Id h)
                                hUnlocked |> set #refOwnedByWorkflow Nothing |> updateRecord
                                Log.info $ "Unlocked h:" ++ show h
                                Log.info $ "version=" ++ show v
                                newVersion :: Version <- fetch (Id v)
                                newVersion |>set #committed True |> updateRecord
                                Log.info $ "commit version v: " ++ show v
                                w <- workflow |> set #workflowStatus "committed" |> updateRecord
                                Log.info $ "commit workflow w: " ++ show w
                                sOld :: [rec] <- query @ rec |> filterWhere (#refHistory,Id h) |>
                                    filterWhereSql(#refValidfromversion,"<> " ++ encodeUtf8( show v)) |>
                                    filterWhere(#refValidthruversion,Nothing) |> fetch
                                case head sOld of
                                    Just sOld -> do
                                            sUpd :: rec <- sOld |> set #refValidthruversion (Just (Id v)) |> updateRecord
                                            Log.info $ "predecessor state terminated" ++ show s
                                    Nothing -> Log.info ("no predecessor state" ::String)
                                case getShadowed accessor wfp of
                                    Nothing -> Log.info ("No version" ::String)
                                    Just (shadow,shadowed) -> do
                                        updated :: [Version]<- sqlQuery "update versions v set ref_shadowedby = ? where id in ? returning * " (v, In shadowed)
                                        forEach updated (\v -> Log.info $ "updated" ++ show v)
                                commitTransaction
                                pure (Left "commit successful")
                                -- redirectTo $ ShowWorkflowAction workflowId
                            Nothing -> do
                                pure $ Right $ "cannot commit: state is null h=" ++ show h ++ "v=" ++ show v 
                        Nothing -> do
                            pure $ Right $ "cannot commit: version is null h=" ++ show h
                    Nothing -> do
                        pure $ Right "cannot commit: history is null"
            Nothing -> do
                pure $ Right "SHOULDN'T: empty progress data"

    createHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> Workflow -> rec -> IO rec
    createHistory accessor workflow state = do
        Log.info $ "createHistory for workflow: " ++ show (get #id workflow)
        history ::History <- newRecord |> set #historyType (get #historyType workflow) |> set #refOwnedByWorkflow (Just $ get #id workflow)|> createRecord
        let historyUUID ::UUID = bubu $ get #id history
                                    where bubu (Id uuid) = uuid
        version :: Version <- newRecord |> set #refHistory (get #id history) |>  set #validfrom (get #validfrom workflow) |> createRecord
        let versionId :: Integer = bubu $ get #id version
                                    where bubu (Id intid) = intid
            histoType = get #historyType workflow
        state ::rec <- state |> set #refHistory (get #id history) |> set #refValidfromversion (get #id version) |> createRecord
        let stateId  = get #id state :: Id rec
            cruS :: CRULog (Id rec) = mkInsertLog stateId
            cruW :: CRULog (Id Workflow) = mkInsertLog $ get #id workflow
            cruH :: CRULog (Id History) = mkInsertLog $ get #id history
            cruV :: CRULog (Id Version) = mkInsertLog $ get #id version
            pl  = [mkPersistenceLogState cruS, WorkflowPL cruW, HistoryPL cruH, VersionPL cruV]
            wfp = setWorkFlowState (WorkflowProgress Nothing Nothing Nothing []) $ 
                Just ((stateKeysDefault {history = Just historyUUID, version = Just versionId, state = Just stateId}) :: StateKeys (Id rec)) 
            progress = toJSON wfp {plog = pl}
        uptodate ::Workflow <- workflow |> set #progress progress |> updateRecord
        Log.info ("hier ist Workflow mit JSON " ++ show (get #progress uptodate))
        pure state 
    
    getShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> WorkflowProgress -> Maybe (Integer,[Integer])
    getShadowed accessor wfp = shadowed $ fromJust $ accessor wfp 
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress

    mutateHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> Workflow -> rec -> IO rec
    mutateHistory accessor workflow state = do
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
        let versionIdMB = getStateVersionIdMB accessor wfprogress
        Log.info $ "mutateHistory Update histoType/wfprogress/versionid" ++ show (get #historyType workflow) ++ show wfprogress ++ "/" ++ show versionIdMB
        case versionIdMB of
            Just v -> do
                Log.info ("mutateHistory Update existing Version" :: String)
                pure state
            Nothing -> do
                Log.info ("mutateHistory Update new Version" :: String)
                let validfrom = tshow $ get #validfrom workflow
                let historyId =  get #refHistory state
                version :: Version <- newRecord |> set #refHistory historyId |> set #validfrom (get #validfrom workflow) |> createRecord
                newState :: rec <- newRecord |> set #refHistory historyId |> set #refValidfromversion (get #id version) |>
                    set #content (get #content state) |> createRecord
                let wfpNew :: Value = updateWfpV accessor wfprogress (fromId historyId) (fromId $ get #id version) (get #id newState )
                workflow :: Workflow <- workflow |> set #progress wfpNew   |> updateRecord  
                Log.info $ "mutateHistory Update new Version = " ++  show (get #id newState)           
                pure newState

    queryImmutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> Id Version -> IO (Id rec)
    queryImmutableState versionId =  do
        mstate <- query @rec |> filterWhere (#refValidfromversion, versionId) |> fetchOne
        pure $ get #id mstate

    queryMutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> (WorkflowProgress ->  Maybe (StateKeys (Id rec)))-> Workflow -> IO (rec,[Version])
    queryMutableState accessor workflow =  do
        mutable :: (Version,[Version]) <- queryVersionMutableValidfrom accessor workflow 
        let h = get #refHistory $ fst mutable
        Log.info $ "queryMutableState " ++ show h
        let v = get #id $ fst mutable
        mstate <- query @rec |> filterWhere(#refHistory, h) |> filterWhereSql (#refValidfromversion, encodeUtf8("<= " ++ show v)) |>
                            queryOr 
                                 (filterWhereSql (#refValidthruversion, encodeUtf8("> " ++ show v)))
                                 (filterWhereSql (#refValidthruversion, "is null")) |> fetchOne
        
        pure (mstate,snd mutable)

    queryVersionMutableValidfrom :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> Workflow -> IO (Version,[Version])
    queryVersionMutableValidfrom accessor workflow = do
        Log.info $ "queryVersionMutableValidfrom Workflow=" ++ show workflow
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
            validfrom = tshow $ get #validfrom workflow
        Log.info $ "workflowProgress:" ++ show wfprogress
        let historyId =  fromJust $ case get #historyType workflow of
                HistorytypeContract -> getStatehistoryIdMB contract wfprogress
                HistorytypePartner -> getStatehistoryIdMB partner wfprogress
                HistorytypeTariff -> getStatehistoryIdMB tariff wfprogress
        Log.info $ "HistoryId:" ++ show historyId ++ "validfrom" ++ show validfrom
        let q :: Query = "SELECT * FROM versions v WHERE v.id in (SELECT max(id) FROM versions where ref_history = ? and validfrom <= ?)"
            p :: (Id History, Text) = (Id historyId, validfrom)
        vs :: [Version]  <- sqlQuery  q p
        let versionId = get #id $ fromJust $ head vs 
        Log.info ( "queryVersionMutableValidfrom versionid=" ++ show versionId )
        let q2 :: Query = "SELECT * FROM versions v WHERE ref_history = ? and v.id > ? and validfrom > ?"
        let p2 :: (Id History, Id Version,Text) = (Id historyId, versionId, validfrom)
        shadowed :: [Version]  <- sqlQuery  q2 p2
        let shadowedIds :: [Integer] = map (getKey . get #id) shadowed  
        Log.info ( "queryVersionMutableValidfrom shadowed=" ++ show shadowed )
        workflow :: Workflow <- setWfp workflow (setShadowed accessor wfprogress (getKey versionId, shadowedIds)) |> updateRecord
        pure (fromJust $ head vs, shadowed)
            where getKey (Id key) = key

    runMutation :: (?modelContext::ModelContext, ?context::context, LoggingProvider context, Show rec, CanVersion rec) => (WorkflowProgress -> Maybe (StateKeys (Id rec))) -> User -> HistoryType -> rec -> Text -> IO()
    runMutation accessor usr histoType rec newContent = do
        Log.info $ ">>>>>>>>>>>>>>> VOR  MUTATE " ++ show histoType 
        wfMUT0 ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType histoType |> set #workflowType WftypeUpdate  |> createRecord
        let h :: (Id History) = get #refHistory rec
            wfp :: WorkflowProgress = case histoType of
                HistorytypeContract -> WorkflowProgress (Just (stateKeysDefault { history = Just $ fromId h } )) Nothing Nothing []
                HistorytypePartner -> WorkflowProgress Nothing (Just (stateKeysDefault { history = Just $ fromId h } )) Nothing []
                HistorytypeTariff -> WorkflowProgress Nothing Nothing (Just (stateKeysDefault { history = Just $ fromId h } )) []
            wfpJ :: Value = fromJust $ decode $ encode $ wfp
        today <- today 
        wfMUT1 <- wfMUT0 |> set #progress wfpJ |> set #validfrom today |> updateRecord
        mutable :: (rec,[Version]) <- queryMutableState accessor wfMUT1
        Log.info $ "MUTABLE=" ++ show (fst mutable)
        let recMUT0 = fst mutable |> set #content newContent
        recMUT :: rec <- mutateHistory getAccessor wfMUT1 recMUT0
        wfMUT :: Workflow <- fetch (get #id wfMUT1)
        Log.info $ ">>>>>>>>>>>>>>> NACH MUTATE" ++ show histoType
        Log.info $ "Workflow für commit:" ++ show  wfMUT
        result <- commitState accessor wfMUT 
        case result of
            Left msg -> Log.info $ "SUCCESS:"++ msg
            Right msg -> Log.info $ "ERROR:" ++ msg
    
        Log.info $ ">>>>>>>>>>>>>>> NACH COMMITMUTATATION " ++ show histoType

instance CanVersion Contract where
    getAccessor :: (WorkflowProgress -> Maybe (StateKeys (Id' "contracts")))
    getAccessor = contract
    mkPersistenceLogState :: CRULog (Id Contract) -> PersistenceLog
    mkPersistenceLogState cru = ContractPL cru
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id' "contracts"))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
    setShadowed accessor wfp shadow = let new :: StateKeys (Id' "contracts") = fromJust $ accessor wfp 
        in wfp {contract = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowProgress -> Maybe (StateKeys (Id' "contracts")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {contract = s} 
instance CanVersion Partner where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id' "partners")))
    getAccessor = partner
    mkPersistenceLogState :: CRULog (Id Partner) -> PersistenceLog
    mkPersistenceLogState cru = PartnerPL cru
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id' "partners"))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
    setShadowed accessor wfp shadow = let new :: StateKeys (Id' "partners") = fromJust $ accessor wfp 
        in wfp {partner = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id' "partners")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {partner = s} 
instance CanVersion Tariff where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id' "tariffs")))
    getAccessor = tariff
    mkPersistenceLogState :: CRULog (Id Tariff) -> PersistenceLog
    mkPersistenceLogState cru = TariffPL cru
    setShadowed :: (WorkflowProgress ->  Maybe (StateKeys (Id' "tariffs"))) -> WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
    setShadowed accessor wfp shadow = let new :: StateKeys (Id' "tariffs") = fromJust $ accessor wfp 
        in wfp {tariff = Just $ new { shadowed = Just shadow }}
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id' "tariffs")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {tariff = s} 
