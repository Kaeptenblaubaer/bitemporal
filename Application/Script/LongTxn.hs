#!/usr/bin/env run-script
module Application.Script.LongTxn where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Exts
import GHC.Generics
import Data.Maybe
import Generated.Types
import Application.Script.Prelude
import Application.Helper.Controller 
import IHP.Log as Log
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal

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
            VersionPL cru -> commit cru
            ContractPL cru -> commit cru
            PartnerPL cru -> commit cru
            TariffPL cru -> commit cru
        putStrLn "Logged nach Commit"

    putStrLn "NACH LOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOGLOG"

