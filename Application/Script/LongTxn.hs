#!/usr/bin/env run-script
module Application.Script.LongTxn where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Application.Helper.CanVersion
import Application.Helper.WorkflowProgress
import Application.Script.Prelude
import IHP.Log as Log
import Data.Maybe
-- 

run :: Script
run = do
    usr :: User <- query @User |> fetchOne 
    wfc ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType HistorytypeContract |> set #workflowType WftypeNew |> createRecord
    wfp ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType HistorytypePartner |> set #workflowType WftypeNew |> createRecord
    wft ::Workflow <- newRecord |> set #refUser (get #id usr) |> set #historyType HistorytypeTariff |> set #workflowType WftypeNew |> createRecord  
    let c0 :: ContractState = newRecord |> set #content "initial"
        p0 :: PartnerState = newRecord |> set #content "initial"
        t0 :: TariffState = newRecord |> set #content "initial"
    c::ContractState <- createHistory contract wfc c0
    ch :: History <- fetch (get #refHistory c)
    cv :: Version <- fetch (get #refValidfromversion c)
    p::PartnerState <- createHistory partner wfp p0
    ph :: History <- fetch (get #refHistory p)
    pv :: Version <- fetch (get #refValidfromversion p)
    t::TariffState <- createHistory tariff wft t0
    th :: History <- fetch (get #refHistory t)
    tv :: Version <- fetch (get #refValidfromversion t)
    Log.info ("=============================================" :: String)
    Log.info $ show c
    Log.info $ show p
    Log.info $ show t
    Log.info ("=============================================" :: String)
    let tlogWC :: CRULog (Id Workflow) = mkInsertLog (get #id wfc)
        tlogWCJ = toJSON tlogWC
        tlogC :: CRULog (Id ContractState) = mkInsertLog (get #id c)
        tlogCJ = toJSON tlogC
        tlogHC :: CRULog (Id History) = mkInsertLog (get #id ch)
        tlogHCJ = toJSON tlogHC
        tlogVC :: CRULog (Id Version) = mkInsertLog (get #id cv)
        tlogVCJ = toJSON tlogVC
        tlogWP :: CRULog (Id Workflow) = mkInsertLog (get #id wfp)
        tlogWPJ = toJSON tlogWC
        tlogP :: CRULog (Id PartnerState) = mkInsertLog (get #id p)
        tlogPJ = toJSON tlogP
        tlogHP :: CRULog (Id History) = mkInsertLog (get #id ph)
        tlogHPJ = toJSON tlogHP
        tlogVP :: CRULog (Id Version) = mkInsertLog (get #id pv)
        tlogVPJ = toJSON tlogVP
        tlogWT :: CRULog (Id Workflow) = mkInsertLog (get #id wft)
        tlogWTJ = toJSON tlogWT
        tlogT :: CRULog (Id TariffState) = mkInsertLog (get #id t)
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

    Log.info ("VOR  COMMIT CONTRACT CONTRACT CONTRACT CONTRACT CONTRACT" ::String)
    wfcUpd <- fetch (get #id wfc)
    commitState contract wfcUpd

    Log.info ("NACH COMMIT CONTRACT CONTRACT CONTRACT CONTRACT CONTRACT" ::String)

    Log.info ("VOR  COMMIT PARTNER  PARTNER  PARTNER  PARTNER  PARTNER" ::String)
    wfpUpd <- fetch (get #id wfp)
    commitState partner wfpUpd

    Log.info ("NACH COMMIT PARTNER  PARTNER  PARTNER  PARTNER  PARTNER" ::String)

    Log.info ("VOR  COMMIT TARIFF   TARIFF   TARIFF   TARIFF   TARIFF" ::String)
    wftUpd <- fetch (get #id wft)
    commitState tariff wftUpd
    
    Log.info ("NACH COMMIT TARIFF   TARIFF   TARIFF   TARIFF   TARIFF" ::String)

    runMutation contract usr HistorytypeContract c "mutatated ContractState"
    runMutation partner usr HistorytypePartner p "mutatated PartnerState"
    runMutation tariff usr HistorytypeTariff t "mutatated TariffState"


--    forEach (persistenceLogC ++ persistenceLogP ++ persistenceLogT) \pl -> do
--        Log.info $ "Logged plog:" ++ show pl
--        case pl of
--            WorkflowPL cru -> commit cru
--            HistoryPL cru -> commit cru
--            VersionPL cru -> commit cru
--            ContractPL cru -> commitState cru
--            PartnerPL cru -> commit cru
--            TariffPL cru -> commit cru
--        Log.info ("Logged nach Commit" :: String)
--

