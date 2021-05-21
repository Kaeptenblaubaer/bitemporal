#!/usr/bin/env run-script
module Application.Script.LongTxn where
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
import Application.Helper.VersionTree


today :: IO (Day) -- :: (year,month,day)
today = getCurrentTime >>= return . utctDay


class (KnownSymbol (GetTableName rec), rec ~ GetModelByTableName (GetTableName rec), PrimaryKey (GetTableName rec) ~ Integer, Record rec, CanCreate rec,Fetchable (QueryBuilder (GetTableName rec))  rec, FromRow rec,
    HasField "id" rec (Id rec), Show (PrimaryKey (GetTableName rec)), HasField "refHistory" rec (Id History),SetField "refHistory" rec (Id History),
    HasField "refValidfromversion" rec (Id Version), SetField "refValidfromversion" rec (Id Version),
    HasField "refValidthruversion" rec (Maybe(Id Version)), SetField "refValidthruversion" rec (Maybe (Id Version)),

    HasField "content" rec Text, SetField "content" rec Text) => CanVersion rec 
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
    initialWfpV :: (WorkflowProgress ->  Maybe (StateKeys (Id rec)) -> WorkflowProgress) -> UUID -> Value
    initialWfpV accessor h = fromJust $ decode $ encode $ setWorkFlowState (WorkflowProgress Nothing Nothing Nothing) (Just (StateKeys {history = (Just h)} ))

    queryMutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> Workflow -> IO (rec,[Version])

    queryMutableState workflow =  do
        
        mutable :: (Version,[Version]) <- queryVersionMutableValidfrom workflow 
        let h = get #refHistory $ fst mutable
        Log.info $ "querymutable " ++ show h
        let v = get #id $ fst mutable
        mstate <- query @rec |> filterWhere(#refHistory, h) |> filterWhereSql (#refValidfromversion, encodeUtf8("<= " ++ (show v))) |>
                            queryOr 
                                 (filterWhereSql (#refValidthruversion, encodeUtf8("> " ++ (show v))))
                                 (filterWhereSql (#refValidthruversion, "is null")) |> fetchOne
        
        pure (mstate,snd mutable)

    queryImmutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> (Id Version) -> IO (Id rec)
    queryImmutableState versionId =  do
        mstate <- query @rec |> filterWhere (#refValidfromversion, versionId) |> fetchOne
        pure $ get #id mstate

    createHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => Workflow -> rec -> IO rec
    createHistory workflow state = do
        Log.info $ "createHistory for workflow: " ++ (show (get #id workflow))
        history ::History <- newRecord |> set #historyType (get #historyType workflow) |> set #refOwnedByWorkflow (Just $ get #id workflow)|> createRecord
        let historyUUID ::UUID = bubu $ get #id history
                                    where bubu (Id uuid) = uuid
        version :: Version <- newRecord |> set #refHistory (get #id history) |>  set #validfrom (get #validfrom workflow) |> createRecord
        let versionId :: Integer = bubu $ get #id version
                                    where bubu (Id intid) = intid
            histoType = get #historyType workflow
            wfp = initialWfpV histoType historyUUID
            progress = toJSON wfp
        state ::rec <- state |> set #refHistory (get #id history) |> set #refValidfromversion (get #id version) |> createRecord
        uptodate ::Workflow <- workflow |> set #progress progress |> updateRecord
        putStrLn ("hier ist Workflow mit JSON " ++ (show (get #progress uptodate)))
        pure state
 
    mutateHistory :: (?modelContext::ModelContext) => Workflow -> rec -> IO rec
    mutateHistory workflow state = do
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
        let versionIdMB = getStateVersionIdMB wfprogress (get #historyType workflow)
        case versionIdMB of
            Just v -> pure state
            Nothing -> do
                putStrLn "mutateHistory Update new Version"
                let validfrom = tshow $ get #validfrom workflow
                let historyId =  get #refHistory state
                version :: Version <- newRecord |> set #refHistory historyId |> set #validfrom (get #validfrom workflow) |> createRecord
                newState :: rec <- newRecord |> set #refHistory historyId |> set #refValidfromversion (get #id version) |>
                    set #content (get #content state) |> createRecord
                workflow <- setWfp workflow ( upd (fromId $ get #id version ) (fromId $ get #id newState ) wfprogress) |> updateRecord                  
                pure newState
                    where upd vid sid workflow = ((setContractId sid).(setContractVersionId vid)) workflow

instance CanVersion Contract where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id' "contracts")))
    getAccessor = (contract)
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id' "contracts")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {contract = s} 
instance CanVersion Partner where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id' "partners")))
    getAccessor = (partner)
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id' "partners")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {partner = s} 
instance CanVersion Tariff where
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id' "tariffs")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {tariff = s} 
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id' "tariffs")))
    getAccessor = (tariff)


queryVersionMutableValidfrom :: (?modelContext::ModelContext) => Workflow -> IO (Version,[Version])
queryVersionMutableValidfrom workflow = do
        putStrLn ( "queryVersionMutableValidfrom Workflow=" ++ (show workflow) )
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
            validfrom = tshow $ get #validfrom workflow
            histoType = get #historyType workflow
            historyId =  fromJust $ getStatehistoryIdMB wfprogress histoType
            q :: Query = "SELECT * FROM versions v WHERE v.id in (SELECT max(id) FROM versions where ref_history = ? and validfrom <= ?)"
            p :: (Id History, Text) = (Id historyId, validfrom)
        vs :: [Version]  <- sqlQuery  q p
        let versionId = get #id $ fromJust $ head vs
        putStrLn ( "queryVersionMutableValidfrom versionid=" ++ (show versionId ))
        let q2 :: Query = "SELECT * FROM versions v WHERE ref_history = ? and v.id > ? and validfrom > ?"
        let p2 :: (Id History, Id Version,Text) = (Id historyId, versionId, validfrom)
        shadowed :: [Version]  <- sqlQuery  q2 p2
        let shadowedIds :: [Integer] = map (getKey .(get #id)) shadowed  
        putStrLn ( "queryVersionMutableValidfrom shadowed=" ++ (show shadowed ))
        workflow :: Workflow <- setWfp workflow (setShadowed wfprogress (getKey versionId, shadowedIds)) |> updateRecord
        pure $ (fromJust $ head vs, shadowed)
            where getKey (Id key) = key

getCurrentWorkflow :: (?context::ControllerContext, ?modelContext::ModelContext) => IO Workflow
getCurrentWorkflow  = do
    id <- getSessionUUID "workflowId"
    putStrLn ("current workflowid = " ++ (show id))
    wf :: Workflow <- fetch (Id (fromJust id))
    pure wf

getCurrentWorkflowId :: (?context::ControllerContext, ?modelContext::ModelContext) => IO (Id Workflow)
getCurrentWorkflowId = do
        workflow :: Workflow <- getCurrentWorkflow
        pure (get #id workflow)
    
setCurrentWorkflowId :: (?context::ControllerContext) => Workflow -> IO ()
setCurrentWorkflowId workflow = do
    oldid <- getSessionUUID "workflowId"
    putStrLn ("old workflowid = " ++ (show oldid))
    setSession "workflowId" (show (get #id workflow)) 
    newid <- getSessionUUID "workflowId"
    putStrLn ("current workflowid = " ++ (show newid))

fromId :: Id' table -> PrimaryKey table
fromId (Id key) = key

data StateKeys stateId = StateKeys  {
    history :: Maybe UUID , version :: Maybe Integer, state :: Maybe stateId, shadowed :: Maybe (Integer,[Integer])} deriving (Generic)
data WorkflowProgress = WorkflowProgress {
    contract :: Maybe (StateKeys (Id' "contracts")), partner:: Maybe (StateKeys (Id' "partners")) , tariff :: Maybe (StateKeys (Id' "tariffs"))
    } deriving (Generic)
instance FromJSON (StateKeys (Id' "contracts"))
instance ToJSON (StateKeys (Id' "contracts"))
instance FromJSON (StateKeys (Id' "partners"))
instance ToJSON (StateKeys (Id' "partners"))
instance FromJSON (StateKeys (Id' "tariffs"))
instance ToJSON (StateKeys (Id' "tariffs"))
instance FromJSON WorkflowProgress
instance ToJSON WorkflowProgress


getWfp :: Workflow -> Maybe WorkflowProgress
getWfp workflow  =  decode $ encode $ get #progress workflow


-- initialWfpV :: HistoryType -> UUID -> Value
-- initialWfpV histoType historyId = fromJust $ decode $ encode $ case histoType of
--     HistorytypeContract -> WorkflowProgress { contract = Just $ StateKeys (Just historyId) Nothing Nothing Nothing , partner = Nothing, tariff = Nothing}
--     HistorytypePartner -> WorkflowProgress {partner = Just $ StateKeys (Just historyId) Nothing Nothing Nothing , contract = Nothing, tariff = Nothing}
--     HistorytypeTariff -> WorkflowProgress {tariff = Just $ StateKeys (Just historyId) Nothing Nothing Nothing, contract = Nothing , partner = Nothing }
-- 
updateWfpV :: Value -> HistoryType -> UUID -> Integer -> Integer -> Value
updateWfpV wfp histoType historyId versionId stateId =
    let stateKeys = Just (StateKeys (Just historyId)  (Just versionId) (Just stateId) Nothing)
        wfp = case histoType of
            HistorytypeContract -> wfp { contract = stateKeys }
            HistorytypePartner -> wfp {partner = stateKeys }
            HistorytypeTariff -> wfp {tariff = stateKeys }
    in fromJust $ decode $ encode wfp

setWfp :: Workflow -> WorkflowProgress -> Workflow
setWfp wf wfp = wf |> set #progress ( fromJust $ decode $ encode wfp )

-- getWorkFlowState :: WorkflowProgress ->  HistoryType ->Maybe StateKeys
-- getWorkFlowState wfp histoType = case histoType of
--             HistorytypeContract -> contract wfp
--             HistorytypePartner ->  partner wfp
--             HistorytypeTariff ->  tariff wfp

getStatehistoryIdMB :: WorkflowProgress -> HistoryType -> Maybe UUID
getStatehistoryIdMB wfp histoType = case getWorkFlowState  wfp histoType of
    Nothing -> Nothing
    Just wfstate -> history wfstate 

getStateVersionIdMB :: WorkflowProgress -> HistoryType -> Maybe Integer
getStateVersionIdMB wfp histoType = case getWorkFlowState  wfp histoType of
    Nothing -> Nothing
    Just wfstate -> version wfstate

getStateIdMB :: WorkflowProgress -> HistoryType -> Maybe Integer
getStateIdMB  wfp histoType = case getWorkFlowState  wfp histoType of
    Nothing -> Nothing
    Just wfstate -> state wfstate

getContractId :: Workflow -> Id Contract
getContractId workflow =  Id $ fromJust $ getStateIdMB (fromJust $ (decode . encode) $ get  #progress workflow) (get #historyType workflow)
setContractVersionId :: Integer -> WorkflowProgress -> WorkflowProgress
setContractVersionId vid (WorkflowProgress (Just (StateKeys h v c sh)) partner tariff) = WorkflowProgress (Just (StateKeys h (Just vid) c sh)) partner tariff

setContractId :: Integer -> WorkflowProgress -> WorkflowProgress
setContractId sid (WorkflowProgress (Just (StateKeys h v c sh)) partner tariff)  = WorkflowProgress (Just (StateKeys h v (Just sid) sh)) partner tariff

getShadowed :: WorkflowProgress -> Maybe (Integer,[Integer])
getShadowed (WorkflowProgress (Just (StateKeys h v c shadowed)) partner tariff) = shadowed

setShadowed :: WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
setShadowed (WorkflowProgress (Just (StateKeys h v c sh)) partner tariff) shadowed = WorkflowProgress (Just (StateKeys h v c (Just shadowed))) partner tariff


data CRULog a = Inserted  { key::a }| Updated { old::a , new :: a } deriving (Generic, Show,Read)
-- instance (ToJSON a) => ToJSON (CRULog a) where
--     toJSON cru = case cru of
--         Inserted key -> object ["tag" .= String "insert", "key" .= key]
--         Updated old new -> object ["tag" .= String "update",  "old" .= old, "new" .= new ]
-- 
instance (ToJSON a) => ToJSON (CRULog a)
instance (FromJSON a) => FromJSON (CRULog a)

data PersistenceLog =
    WorkflowPL (CRULog (Id Workflow)) | HistoryPL (CRULog (Id History)) | VersionPL (CRULog (Id Version)) | ContractPL (CRULog (Id Contract)) | PartnerPL (CRULog (Id Partner))| TariffPL (CRULog (Id Tariff))
    deriving (Generic, Show)

instance ToJSON PersistenceLog
instance FromJSON PersistenceLog

instance ToJSON (Id' "workflows") where
    toJSON (Id key) = object ["workflowId" .= key]
instance FromJSON (Id' "workflows") where
    parseJSON = withObject "workflowId" $ \o ->
        Id <$> o .: "workflowId"
instance ToJSON (Id' "histories") where
    toJSON (Id key) = object ["historyId" .= key]
instance FromJSON (Id' "histories") where
    parseJSON = withObject "historyId" $ \o ->
        Id <$> o .: "historyId"
instance ToJSON (Id' "versions") where
    toJSON (Id key) = object ["versionId" .= key]
instance FromJSON (Id' "versions") where
    parseJSON = withObject "versionId" $ \o ->
        Id <$> o .: "versionId"
instance ToJSON (Id' "contracts") where
    toJSON (Id key) = object ["contractId" .= key]
instance FromJSON (Id' "contracts") where
    parseJSON = withObject "contractId" $ \o ->
        Id <$> o .: "contractId"
instance ToJSON (Id' "partners") where
    toJSON (Id key) = object ["partnerId" .= key]
instance FromJSON (Id' "partners") where
    parseJSON = withObject "partnerId" $ \o ->
        Id <$> o .: "partnerId"
instance ToJSON (Id' "tariffs") where
    toJSON (Id key) = object ["tariffId" .= key]
instance FromJSON (Id' "tariffs") where
    parseJSON = withObject "tariffId" $ \o ->
        Id <$> o .: "tariffId"

class (KnownSymbol (GetTableName rec), rec ~ GetModelByTableName (GetTableName rec), Record rec, FilterPrimaryKey (GetTableName rec),CanCreate rec,Fetchable (QueryBuilder (GetTableName rec))  rec, FromRow rec,
    HasField "id" rec (Id rec), Show rec, Show (PrimaryKey (GetTableName rec)) , ToJSON (PrimaryKey (GetTableName rec))) => HasTxnLog rec 
    where
    mkInsertLog :: rec -> CRULog (Id rec)
    mkInsertLog row = Inserted $ get #id row
    mkUpdateLog :: rec -> rec -> CRULog (Id rec)
    mkUpdateLog old new = Updated (get #id old) (get #id new)
    commit ::  (?modelContext::ModelContext, ?context::context, LoggingProvider context )  => CRULog (Id rec) -> IO()
    commit cruLog = do
        case cruLog of
            Inserted key -> do
                rec <- fetch key
                putStrLn $ "huhu INSERTED " ++ show rec
            Updated old new -> do
                recOld <- fetch old
                putStrLn $ "huhu OLD " ++ show recOld
                recNew <- fetch old
                putStrLn $ "huhu OLD " ++ show recNew

instance HasTxnLog Workflow
instance HasTxnLog History 
instance HasTxnLog Version 
instance HasTxnLog Contract
instance HasTxnLog Partner
instance HasTxnLog Tariff

getPLOG :: Workflow -> Maybe [PersistenceLog]
getPLOG workflow  =  decode $ encode $ get #progress workflow

run :: Script
run = do
    usr :: User <- query @User |> fetchOne 
    wf ::Workflow <- newRecord |> set #refUser (get #id usr) |> createRecord
    h :: History <- newRecord |> set #refOwnedByWorkflow (Just $ get #id wf) |> createRecord
    v :: Version <- newRecord |> set #refHistory (get #id h )|> createRecord  
    c :: Contract <- newRecord |> set #refHistory (get #id h) |> set #refValidfromversion (get #id v )|> createRecord
    p :: Partner <- newRecord |> set #refHistory (get #id h) |> set #refValidfromversion (get #id v )|> createRecord
    t :: Tariff <- newRecord |> set #refHistory (get #id h) |> set #refValidfromversion (get #id v )|> createRecord
    let tlogW :: CRULog (Id Workflow) = mkInsertLog wf
        tlogWJ = toJSON tlogW
        tlogH :: CRULog (Id History) = mkInsertLog h
        tlogHJ = toJSON tlogH
        tlogV :: CRULog (Id Version) = mkInsertLog v
        tlogVJ = toJSON tlogV
        tlogC :: CRULog (Id Contract) = mkInsertLog c
        tlogCJ = toJSON tlogC
        tlogP :: CRULog (Id Partner) = mkInsertLog p
        tlogPJ = toJSON tlogP
        tlogT :: CRULog (Id Tariff) = mkInsertLog t
        tlogTJ = toJSON tlogT
        persistenceLog :: [PersistenceLog] =[ TariffPL tlogT, PartnerPL tlogP, ContractPL tlogC, VersionPL tlogV, HistoryPL tlogH, WorkflowPL tlogW ]
        persistenceLogJ = toJSON persistenceLog
    putStrLn $ show  tlogWJ
    putStrLn $ show  tlogHJ
    putStrLn $ show  tlogVJ
    putStrLn $ show  tlogCJ
    putStrLn $ show  tlogPJ
    putStrLn $ show  tlogTJ
    v2 :: Version <- fetch $ key tlogV
    putStrLn $ show v2
    putStrLn "============================================="
    putStrLn $ show persistenceLog
    putStrLn "---------------------------------------------"
    putStrLn $ show persistenceLogJ
    putStrLn "============================================="
    wf <- wf |> set #progress persistenceLogJ |> updateRecord
    wf :: Workflow <- fetch $ get #id wf 
    let plogMB  = getPLOG wf
        plog = fromJust plogMB
    putStrLn "************************************************"
    putStrLn $ "SHOWING plog of " ++ show (get #id wf) ++ show plog
    putStrLn "************************************************"
    putStrLn "VOR  LOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOG"

    forEach plog \pl -> do
        putStrLn $ "Logged plog:" ++ show pl
        case pl of
            WorkflowPL cru -> commit cru
            HistoryPL cru -> commit cru
            VersionPL cru -> commit cru
            ContractPL cru -> commit cru
            PartnerPL cru -> commit cru
            TariffPL cru -> commit cru
        putStrLn "Logged nach Commit"

    putStrLn "NACH LOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOG"

