module Application.Helper.WorkflowProgress where
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

getCurrentWorkflow :: (?context::ControllerContext, ?modelContext::ModelContext) => IO Workflow
getCurrentWorkflow  = do
    id <- getSessionUUID "workflowId"
    Log.info $ "current workflowid = " ++ show id
    wf :: Workflow <- fetch (Id (fromJust id))
    pure wf

getCurrentWorkflowId :: (?context::ControllerContext, ?modelContext::ModelContext) => IO (Id Workflow)
getCurrentWorkflowId = do
        workflow :: Workflow <- getCurrentWorkflow
        pure (get #id workflow)
    
setCurrentWorkflowId :: (?context::ControllerContext) => Workflow -> IO ()
setCurrentWorkflowId workflow = do
    oldid <- getSessionUUID "workflowId"
    Log.info $ "old workflowid = " ++ show oldid
    setSession "workflowId" (show (get #id workflow)) 
    newid <- getSessionUUID "workflowId"
    Log.info $ "current workflowid = " ++ show newid

fromId :: Id' table -> PrimaryKey table
fromId (Id key) = key

data StateKeys stateId = StateKeys  {
    history :: Maybe UUID , version :: Maybe Integer, state :: Maybe stateId, shadowed :: Maybe (Integer,[Integer])} deriving (Show, Generic)
stateKeysDefault = StateKeys Nothing Nothing Nothing Nothing 

data WorkflowProgress = WorkflowProgress {
    contract :: Maybe (StateKeys (Id' "contract_states")), partner:: Maybe (StateKeys (Id' "partner_states")) , tariff :: Maybe (StateKeys (Id' "tariff_states")),
    plog :: [PersistenceLog]
    } deriving (Show, Generic)
instance FromJSON (StateKeys (Id' "contract_states"))
instance ToJSON (StateKeys (Id' "contract_states"))
instance FromJSON (StateKeys (Id' "partner_states"))
instance ToJSON (StateKeys (Id' "partner_states"))
instance FromJSON (StateKeys (Id' "tariff_states"))
instance ToJSON (StateKeys (Id' "tariff_states"))
instance FromJSON WorkflowProgress
instance ToJSON WorkflowProgress

workflowProgressDefault = ( WorkflowProgress (Just stateKeysDefault) (Just stateKeysDefault) (Just stateKeysDefault) [] )

getWfp :: Workflow -> Maybe WorkflowProgress
getWfp workflow  =  decode $ encode $ get #progress workflow

setWfp :: Workflow -> WorkflowProgress -> Workflow
setWfp wf wfp = wf |> set #progress ( fromJust $ decode $ encode wfp )

data CRULog a = Inserted  { key::a }| Updated { old::a , new :: a } deriving (Generic, Show,Read)
instance (ToJSON a) => ToJSON (CRULog a)
instance (FromJSON a) => FromJSON (CRULog a)

data PersistenceLog =
    WorkflowPL (CRULog (Id Workflow)) | HistoryPL (CRULog (Id History)) | VersionPL (CRULog (Id Version)) | ContractPL (CRULog (Id ContractState)) | PartnerPL (CRULog (Id PartnerState))| TariffPL (CRULog (Id TariffState))
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
instance ToJSON (Id' "contract_states") where
    toJSON (Id key) = object ["contractStateId" .= key]
instance FromJSON (Id' "contract_states") where
    parseJSON = withObject "contractStateId" $ \o ->
        Id <$> o .: "contractStateId"
instance ToJSON (Id' "partner_states") where
    toJSON (Id key) = object ["partnerStateId" .= key]
instance FromJSON (Id' "partner_states") where
    parseJSON = withObject "partnerStateId" $ \o ->
        Id <$> o .: "partnerId"
instance ToJSON (Id' "tariff_states") where
    toJSON (Id key) = object ["tariffStateId" .= key]
instance FromJSON (Id' "tariff_states") where
    parseJSON = withObject "tariffStateId" $ \o ->
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
instance HasTxnLog ContractState
instance HasTxnLog PartnerState
instance HasTxnLog TariffState

getPLOG :: Workflow -> Maybe [PersistenceLog]
getPLOG workflow  =  decode $ encode $ get #progress workflow
