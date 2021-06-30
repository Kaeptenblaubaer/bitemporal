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
    csk::(ContractState, StateKeys (Id Contract)(Id ContractState)) <- createHistory contract wfc c0
    Log.info $ show $ snd csk
    result <- fetch (get #id wfc) >>= (\s -> commitState contract s)
    Log.info $ show result
    psk::(PartnerState, StateKeys (Id Partner)(Id PartnerState)) <- createHistory partner wfp p0
    Log.info $ show $ snd psk
    result <- fetch (get #id wfp) >>= (\s -> commitState partner s)
    Log.info $ show result
    tsk::(TariffState, StateKeys (Id Tariff)(Id TariffState)) <- createHistory tariff wft t0
    Log.info $ show $ snd csk
    result <- fetch (get #id wft) >>= (\s -> commitState tariff s)
    Log.info $ show result

    --ch :: History <- getStateHistoryIdMB contract csk
    --cv :: Version <- getStateVesrionIdMB contract csk
    -- psk::(Partner, StateKeys (Id Partner)(Id PartnerState)) <- createHistory partner wfp p0
    -- ph :: History <- getStateHistoryIdMB partner psk
    -- pv :: Version <- getStateVersionIdMB partner psk
    -- tsk::(Tariff, StateKeys (Id Tariff)(Id TariffState)) <- createHistory tariff wft t0
    -- th :: History <- getStateHistoryIdMB tatiff tsk
    -- tv :: Version <- getStateVersionIdMB tariff tsk
--    Log.info ("=============================================" :: String)
--    Log.info $ show c
--    Log.info $ show p
--    Log.info $ show t
--    Log.info ("=============================================" :: String)
--    let tlogWC :: CRULog (Id Workflow) = mkInsertLog (get #id wfc)
--        tlogWCJ = toJSON tlogWC
--        tlogC :: CRULog (Id ContractState) = mkInsertLog (get #id c)
--        tlogCJ = toJSON tlogC
--        tlogHC :: CRULog (Id History) = mkInsertLog (get #id ch)
--        tlogHCJ = toJSON tlogHC
--        tlogVC :: CRULog (Id Version) = mkInsertLog (get #id cv)
--        tlogVCJ = toJSON tlogVC
--        tlogWP :: CRULog (Id Workflow) = mkInsertLog (get #id wfp)
--        tlogWPJ = toJSON tlogWC
--        tlogP :: CRULog (Id PartnerState) = mkInsertLog (get #id p)
--        tlogPJ = toJSON tlogP
--        tlogHP :: CRULog (Id History) = mkInsertLog (get #id ph)
--        tlogHPJ = toJSON tlogHP
--        tlogVP :: CRULog (Id Version) = mkInsertLog (get #id pv)
--        tlogVPJ = toJSON tlogVP
--        tlogWT :: CRULog (Id Workflow) = mkInsertLog (get #id wft)
--        tlogWTJ = toJSON tlogWT
--        tlogT :: CRULog (Id TariffState) = mkInsertLog (get #id t)
--        tlogTJ = toJSON tlogC
--        tlogHT :: CRULog (Id History) = mkInsertLog (get #id th)
--        tlogHTJ = toJSON tlogHT
--        tlogVT :: CRULog (Id Version) = mkInsertLog (get #id tv)
--        tlogVTJ = toJSON tlogVT
--        persistenceLogC :: [PersistenceLog] =[ ContractStatePL tlogC, VersionPL tlogVC, HistoryPL tlogHC, WorkflowPL tlogWC]
--        persistenceLogCJ = toJSON persistenceLogC
--        persistenceLogP :: [PersistenceLog] =[ PartnerStatePL tlogP, VersionPL tlogVP, HistoryPL tlogHP, WorkflowPL tlogWP]
--        persistenceLogPJ = toJSON persistenceLogP
--        persistenceLogT :: [PersistenceLog] =[ TariffStatePL tlogT, VersionPL tlogVT, HistoryPL tlogHT, WorkflowPL tlogWT]
--        persistenceLogTJ = toJSON persistenceLogT
--    Log.info ("============================================="::String)
--    Log.info $ show persistenceLogC
--    Log.info $ show persistenceLogP
--    Log.info $ show persistenceLogT
--    Log.info ("---------------------------------------------" :: String)
--    Log.info $ show persistenceLogCJ
--    Log.info $ show persistenceLogPJ
--    Log.info $ show persistenceLogTJ
--    Log.info ("============================================="::String)
--
--    Log.info ("VOR  COMMIT CONTRACT CONTRACT CONTRACT CONTRACT CONTRACT" ::String)
--    wfcUpd <- fetch (get #id wfc)
--    commitState contract wfcUpd
--
--    Log.info ("NACH COMMIT CONTRACT CONTRACT CONTRACT CONTRACT CONTRACT" ::String)
--
--    Log.info ("VOR  COMMIT PARTNER  PARTNER  PARTNER  PARTNER  PARTNER" ::String)
--    wfpUpd <- fetch (get #id wfp)
--    commitState partner wfpUpd
--
--    Log.info ("NACH COMMIT PARTNER  PARTNER  PARTNER  PARTNER  PARTNER" ::String)
--
--    Log.info ("VOR  COMMIT TARIFF   TARIFF   TARIFF   TARIFF   TARIFF" ::String)
--    wftUpd <- fetch (get #id wft)
--    commitState tariff wftUpd
--    
--    Log.info ("NACH COMMIT TARIFF   TARIFF   TARIFF   TARIFF   TARIFF" ::String)
--
    runMutation contract usr HistorytypeContract (fst csk) "mutatated ContractState"
    runMutation partner usr HistorytypePartner (fst psk) "mutatated PartnerState"
    runMutation tariff usr HistorytypeTariff (fst tsk) "mutatated TariffState"


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

