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
    updateWfpV :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> WorkflowProgress -> Integer -> (Id rec) -> Value
    updateWfpV accessor wfp v s = fromJust $ decode $ encode $ setWorkFlowState wfp (Just (stateKeysDefault { version= Just v, state= Just s } ))
    getStatehistoryIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> WorkflowProgress -> Maybe UUID
    getStatehistoryIdMB accessor wfp = maybe Nothing history (accessor wfp)
    getStateVersionIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) ->WorkflowProgress -> Maybe Integer
    getStateVersionIdMB accessor wfp = maybe Nothing version (accessor wfp)
    getStateIdMB :: (WorkflowProgress ->  Maybe (StateKeys (Id rec))) ->WorkflowProgress -> Maybe (Id rec)
    getStateIdMB  accessor wfp = maybe Nothing state (accessor wfp)

    mkPersistenceLogState :: CRULog (Id rec) -> PersistenceLog

    queryMutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> (WorkflowProgress ->  Maybe (StateKeys (Id rec)))-> Workflow -> IO (rec,[Version])
    queryMutableState accessor workflow =  do
        mutable :: (Version,[Version]) <- queryVersionMutableValidfrom accessor workflow 
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

    createHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> Workflow -> rec -> IO rec
    createHistory accessor workflow state = do
        Log.info $ "createHistory for workflow: " ++ (show (get #id workflow))
        history ::History <- newRecord |> set #historyType (get #historyType workflow) |> set #refOwnedByWorkflow (Just $ get #id workflow)|> createRecord
        let historyUUID ::UUID = bubu $ get #id history
                                    where bubu (Id uuid) = uuid
        version :: Version <- newRecord |> set #refHistory (get #id history) |>  set #validfrom (get #validfrom workflow) |> createRecord
        let versionId :: Integer = bubu $ get #id version
                                    where bubu (Id intid) = intid
            histoType = get #historyType workflow
        state ::rec <- state |> set #refHistory (get #id history) |> set #refValidfromversion (get #id version) |> createRecord
        let stateId  = get #id state :: Id rec
            wfp = setWorkFlowState (WorkflowProgress Nothing Nothing Nothing) $ Just ((stateKeysDefault {history = Just historyUUID, version = Just versionId, state = Just stateId }) :: StateKeys (Id rec))
            progress = toJSON wfp
            cruS :: CRULog (Id rec) = mkInsertLog stateId
            cruW :: CRULog (Id Workflow) = mkInsertLog $ get #id workflow
            cruH :: CRULog (Id History) = mkInsertLog $ get #id history
            cruV :: CRULog (Id Version) = mkInsertLog $ get #id version
            pl  = [mkPersistenceLogState cruS, WorkflowPL cruW, HistoryPL cruH, VersionPL cruV]
        uptodate ::Workflow <- workflow |> set #progress progress |> updateRecord
        Log.info ("hier ist Workflow mit JSON " ++ (show (get #progress uptodate)))
        pure state 
 
    mutateHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> Workflow -> rec -> IO rec
    mutateHistory accessor workflow state = do
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
        let versionIdMB = getStateVersionIdMB accessor wfprogress
        case versionIdMB of
            Just v -> pure state
            Nothing -> do
                Log.info ("mutateHistory Update new Version" :: String)
                let validfrom = tshow $ get #validfrom workflow
                let historyId =  get #refHistory state
                version :: Version <- newRecord |> set #refHistory historyId |> set #validfrom (get #validfrom workflow) |> createRecord
                newState :: rec <- newRecord |> set #refHistory historyId |> set #refValidfromversion (get #id version) |>
                    set #content (get #content state) |> createRecord
                let wfpNew :: Value = updateWfpV accessor wfprogress (fromId $ get #id version) (get #id newState )
                workflow :: Workflow <- workflow |> set #progress wfpNew   |> updateRecord                  
                pure newState

    queryVersionMutableValidfrom :: (?modelContext::ModelContext, ?context::context, LoggingProvider context) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) -> Workflow -> IO (Version,[Version])
    queryVersionMutableValidfrom accessor workflow = do
        Log.info $ "queryVersionMutableValidfrom Workflow=" ++ show workflow
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
            validfrom = tshow $ get #validfrom workflow
            -- histoType = get #historyType workflow
            historyId =  fromJust $ getStatehistoryIdMB accessor wfprogress
            q :: Query = "SELECT * FROM versions v WHERE v.id in (SELECT max(id) FROM versions where ref_history = ? and validfrom <= ?)"
            p :: (Id History, Text) = (Id historyId, validfrom)
        vs :: [Version]  <- sqlQuery  q p
        let versionId = get #id $ fromJust $ head vs
        Log.info ( "queryVersionMutableValidfrom versionid=" ++ (show versionId ))
        let q2 :: Query = "SELECT * FROM versions v WHERE ref_history = ? and v.id > ? and validfrom > ?"
        let p2 :: (Id History, Id Version,Text) = (Id historyId, versionId, validfrom)
        shadowed :: [Version]  <- sqlQuery  q2 p2
        let shadowedIds :: [Integer] = map (getKey .(get #id)) shadowed  
        Log.info ( "queryVersionMutableValidfrom shadowed=" ++ (show shadowed ))
        workflow :: Workflow <- setWfp workflow (setShadowed wfprogress (getKey versionId, shadowedIds)) |> updateRecord
        pure $ (fromJust $ head vs, shadowed)
            where getKey (Id key) = key

instance CanVersion Contract where
    getAccessor :: (WorkflowProgress -> Maybe (StateKeys (Id' "contracts")))
    getAccessor = (contract)
    mkPersistenceLogState :: CRULog (Id Contract) -> PersistenceLog
    mkPersistenceLogState cru = ContractPL cru
    setWorkFlowState :: WorkflowProgress -> Maybe (StateKeys (Id' "contracts")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {contract = s} 
instance CanVersion Partner where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id' "partners")))
    getAccessor = (partner)
    mkPersistenceLogState :: CRULog (Id Partner) -> PersistenceLog
    mkPersistenceLogState cru = PartnerPL cru
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id' "partners")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {partner = s} 
instance CanVersion Tariff where
    getAccessor :: (WorkflowProgress ->Maybe (StateKeys (Id' "tariffs")))
    getAccessor = (tariff)
    mkPersistenceLogState :: CRULog (Id Tariff) -> PersistenceLog
    mkPersistenceLogState cru = TariffPL cru
    setWorkFlowState :: WorkflowProgress ->Maybe (StateKeys (Id' "tariffs")) -> WorkflowProgress
    setWorkFlowState wfp s = wfp  {tariff = s} 

getCurrentWorkflow :: (?context::ControllerContext, ?modelContext::ModelContext) => IO Workflow
getCurrentWorkflow  = do
    id <- getSessionUUID "workflowId"
    Log.info ("current workflowid = " ++ (show id))
    wf :: Workflow <- fetch (Id (fromJust id))
    pure wf

getCurrentWorkflowId :: (?context::ControllerContext, ?modelContext::ModelContext) => IO (Id Workflow)
getCurrentWorkflowId = do
        workflow :: Workflow <- getCurrentWorkflow
        pure (get #id workflow)
    
setCurrentWorkflowId :: (?context::ControllerContext) => Workflow -> IO ()
setCurrentWorkflowId workflow = do
    oldid <- getSessionUUID "workflowId"
    Log.info ("old workflowid = " ++ (show oldid))
    setSession "workflowId" (show (get #id workflow)) 
    newid <- getSessionUUID "workflowId"
    Log.info ("current workflowid = " ++ (show newid))

fromId :: Id' table -> PrimaryKey table
fromId (Id key) = key

data StateKeys stateId = StateKeys  {
    history :: Maybe UUID , version :: Maybe Integer, state :: Maybe stateId, shadowed :: Maybe (Integer,[Integer])} deriving (Generic)
stateKeysDefault = StateKeys Nothing Nothing Nothing Nothing 

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

setWfp :: Workflow -> WorkflowProgress -> Workflow
setWfp wf wfp = wf |> set #progress ( fromJust $ decode $ encode wfp )


getShadowed :: WorkflowProgress -> Maybe (Integer,[Integer])
getShadowed (WorkflowProgress (Just (StateKeys h v c shadowed)) partner tariff) = shadowed

setShadowed :: WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
setShadowed (WorkflowProgress (Just (StateKeys h v c sh)) partner tariff) shadowed = WorkflowProgress (Just (StateKeys h v c (Just shadowed))) partner tariff

data CRULog a = Inserted  { key::a }| Updated { old::a , new :: a } deriving (Generic, Show,Read)
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
    mkInsertLog :: Id rec -> CRULog (Id rec)
    mkInsertLog id = Inserted id
    mkUpdateLog :: rec -> rec -> CRULog (Id rec)
    mkUpdateLog old new = Updated (get #id old) (get #id new)
    commit ::  (?modelContext::ModelContext, ?context::context, LoggingProvider context )  => CRULog (Id rec) -> IO()
    commit cruLog = do
        case cruLog of
            Inserted key -> do
                rec <- fetch key
                Log.info $ "huhu INSERTED " ++ show rec
            Updated old new -> do
                recOld <- fetch old
                Log.info $ "huhu OLD " ++ show recOld
                recNew <- fetch old
                Log.info $ "huhu OLD " ++ show recNew

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
    wfc ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType HistorytypeContract |> createRecord
    wfp ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType HistorytypePartner |> createRecord
    wft ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType HistorytypeTariff |> createRecord  
    let c0 :: Contract = newRecord
        p0 :: Partner = newRecord
        t0 :: Tariff = newRecord
    c::Contract <- createHistory (contract) wfc c0
    ch :: History <- fetch (get #refHistory c)
    cv :: Version <- fetch (get #refValidfromversion c)
    p::Partner <- createHistory (partner) wfp p0
    ph :: History <- fetch (get #refHistory p)
    pv :: Version <- fetch (get #refValidfromversion p)
    t::Tariff <- createHistory (tariff) wft t0
    th :: History <- fetch (get #refHistory t)
    tv :: Version <- fetch (get #refValidfromversion t)
    Log.info ("=============================================" :: String)
    Log.info $ show c
    Log.info $ show p
    Log.info $ show t
    Log.info ("=============================================" :: String)
    let tlogWC :: CRULog (Id Workflow) = mkInsertLog (get #id wfc)
        tlogWCJ = toJSON tlogWC
        tlogC :: CRULog (Id Contract) = mkInsertLog (get #id c)
        tlogCJ = toJSON tlogC
        tlogHC :: CRULog (Id History) = mkInsertLog (get #id ch)
        tlogHCJ = toJSON tlogHC
        tlogVC :: CRULog (Id Version) = mkInsertLog (get #id cv)
        tlogVCJ = toJSON tlogVC
        tlogWP :: CRULog (Id Workflow) = mkInsertLog (get #id wfp)
        tlogWPJ = toJSON tlogWC
        tlogP :: CRULog (Id Partner) = mkInsertLog (get #id p)
        tlogPJ = toJSON tlogP
        tlogHP :: CRULog (Id History) = mkInsertLog (get #id ph)
        tlogHPJ = toJSON tlogHP
        tlogVP :: CRULog (Id Version) = mkInsertLog (get #id pv)
        tlogVPJ = toJSON tlogVP
        tlogWT :: CRULog (Id Workflow) = mkInsertLog (get #id wft)
        tlogWTJ = toJSON tlogWT
        tlogT :: CRULog (Id Tariff) = mkInsertLog (get #id t)
        tlogTJ = toJSON tlogC
        tlogHT :: CRULog (Id History) = mkInsertLog (get #id th)
        tlogHTJ = toJSON tlogHT
        tlogVT :: CRULog (Id Version) = mkInsertLog (get #id tv)
        tlogVTJ = toJSON tlogVT
        persistenceLogC :: [PersistenceLog] =[ ContractPL tlogC, VersionPL tlogVC, HistoryPL tlogHC, WorkflowPL tlogWC]
        persistenceLogCJ = toJSON persistenceLogC
        persistenceLogP :: [PersistenceLog] =[ PartnerPL tlogP, VersionPL tlogVP, HistoryPL tlogHP, WorkflowPL tlogWP]
        persistenceLogPJ = toJSON persistenceLogP
        persistenceLogT :: [PersistenceLog] =[ TariffPL tlogT, VersionPL tlogVT, HistoryPL tlogHT, WorkflowPL tlogWT]
        persistenceLogTJ = toJSON persistenceLogT
    Log.info ("============================================="::String)
    Log.info $ show persistenceLogC
    Log.info $ show persistenceLogP
    Log.info $ show persistenceLogT
    Log.info ("---------------------------------------------" :: String)
    Log.info $ show persistenceLogCJ
    Log.info $ show persistenceLogPJ
    Log.info $ show persistenceLogTJ
    Log.info ("============================================="::String)

    Log.info ("NACH LOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOG" ::String)
    forEach (persistenceLogC ++ persistenceLogP ++ persistenceLogT) \pl -> do
        Log.info $ "Logged plog:" ++ show pl
        case pl of
            WorkflowPL cru -> commit cru
            HistoryPL cru -> commit cru
            VersionPL cru -> commit cru
            ContractPL cru -> commit cru
            PartnerPL cru -> commit cru
            TariffPL cru -> commit cru
        Log.info ("Logged nach Commit" :: String)

    Log.info (")NACH LOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOG" :: String)
