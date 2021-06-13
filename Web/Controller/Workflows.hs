module Web.Controller.Workflows where

import Web.Controller.Prelude
import Web.View.Workflows.Index
import Web.View.Workflows.New
import Web.View.Workflows.Edit
import Web.View.Workflows.Show
import Web.Controller.Histories
import Application.Helper.CanVersion (createHistory, getKey, queryMutableState, today )
import Data.Maybe ( fromJust )
import Data.ByteString.Lazy as BSL (ByteString,fromStrict)
import Data.Text.Encoding(encodeUtf8)
import IHP.Log as Log

instance Controller WorkflowsController where
    action WorkflowsAction = do
        -- let validfrom = paramOrNothing @Day "validfrom"
        workflows <- query @Workflow |> fetch
        render IndexView { .. }

-- Types of workflow
--  Creation of a new Entity. No historyId given
--  Mutation of an existing one, designated by given historyId
--      on successful locking of the entity creation of the workflow succeeds
--      locking fails, if the entity is already marked as locked by another workflow (#refOwnedByWorkflow)
--      or if the data base row is locked by a concurrent process  

    action NewWorkflowAction = do
        today <- today
        now <- getCurrentTime
        user <- query @User |> fetchOne 
        let historyIdMB = paramOrNothing @UUID "historyId"
        case historyIdMB of
            Nothing -> do
                Log.info ("New Creation Workflow" :: String)
                let initialProgress = WorkflowProgress Nothing Nothing Nothing []
                    initialProgressV = fromJust $ decode $ encode initialProgress
                    workflow :: Workflow = newRecord |> set #refUser (get #id user) |> set #validfrom today |>
                        set #workflowType WftypeNew |> set #progress initialProgressV
                setModal NewView { .. }
            Just historyId ->  do
                Log.info $ "New Mutation Workflow for History:" ++ show historyId
                mylockedHistory :: [History] <- sqlQuery "SELECT * FROM histories WHERE id = ? AND ref_owned_by_workflow is null FOR UPDATE SKIP LOCKED" (Only historyId)
                case head mylockedHistory of
                    Nothing -> do
                        Log.info $ "History cannot be locked: " ++ show historyId
                        theirlockedHistory :: [History] <- sqlQuery "SELECT * FROM histories WHERE id = ? FOR UPDATE SKIP LOCKED" (Only historyId)
                        case head theirlockedHistory of
                            Nothing -> do
                                setErrorMessage $ "History is being locked by s.o. else: " ++ show historyId
                                redirectTo $ ShowHistoryAction (Id historyId)
                            Just h -> do
                                setErrorMessage $ "History " ++ show historyId ++  " has been locked by Workflow " ++ show (get #refOwnedByWorkflow h)
                                redirectTo $ ShowWorkflowAction $ fromJust (get #refOwnedByWorkflow h)

                    Just h -> do
                        Log.info $ "History will be locked: " ++ show historyId
                        let hType = get #historyType h
                            initialProgress :: Value = case hType of
                                HistorytypeContract -> initialWfpV contract historyId 
                                HistorytypePartner ->  initialWfpV partner historyId 
                                HistorytypeTariff ->  initialWfpV tariff historyId 
                            workflow = newRecord |> set #refUser (get #id user) |> set #validfrom today |> set #workflowType WftypeUpdate |>
                                set #historyType hType |> set #progress initialProgress
                        setModal NewView { .. }
        jumpToAction WorkflowsAction

    action ShowWorkflowAction { workflowId } = do
        workflow :: Workflow <- fetch workflowId
        setSession "workflowId" (show workflowId)
        Log.info $ "Show Workflow. Set Session workflowId= " ++ show workflowId
        let histoType = get #historyType workflow
        Log.info $ "HistoryType= " ++ show histoType
        let wfp = fromJust $ getWfp workflow
        case histoType of
            HistorytypeContract -> renderShowView contract workflow
            HistorytypePartner -> renderShowView partner workflow
            HistorytypeTariff -> renderShowView tariff workflow
        where renderShowView accessor workflow = do
                let wfp = fromJust $ getWfp workflow
                    stateHistoryIdMB = getStatehistoryIdMB accessor wfp 
                    stateVersionIdMB = getStateVersionIdMB accessor wfp 
                    stateIdMB = case getStateIdMB accessor wfp of
                        Nothing -> Nothing
                        Just (Id key) -> Just key
                render ShowView { .. }


    action EditWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        render EditView { .. }

    action UpdateWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        workflow
            |> buildWorkflow
            |> ifValid \case
                Left workflow -> render EditView { .. }
                Right workflow -> do
                    workflow <- workflow |> updateRecord
                    setSuccessMessage "Workflow updated"
                    redirectTo EditWorkflowAction { .. }

    action CreateWorkflowAction = do
        Log.info ("creating Workflow":: String)
        let workflow = newRecord @Workflow
        workflow
            |> buildWorkflow
            |> ifValid \case
                Left workflow -> render NewView { .. }
                Right workflow -> do
                    workflow <- workflow |> createRecord 
                    setCurrentWorkflowId workflow
                    Log.info $ "wfnew sessionset =" ++ show workflow
                    setSuccessMessage "Workflow created"
                    Log.info $ "redirecting to NextWorkflowAction " ++ show (get #id workflow)
                    redirectTo $ NextWorkflowAction $ get #id workflow

    action DeleteWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        deleteRecord workflow
        setSuccessMessage "Workflow deleted"
        redirectTo WorkflowsAction

    action NextWorkflowAction { workflowId } = do
        Log.info $ "nextaction Workflow" ++ show workflowId
        workflow <- getCurrentWorkflow
        putStrLn ("NextWF wf="++ show workflow)
        case getWfp workflow of
            Just wfp ->case get #workflowType workflow of
                WftypeNew -> case get #historyType workflow of
                    HistorytypeContract -> case getStateIdMB contract wfp of
                        Nothing -> redirectTo NewContractAction 
                        Just sid -> redirectTo $ EditContractAction sid
                    HistorytypePartner -> case getStateIdMB partner wfp of
                        Nothing -> redirectTo NewPartnerAction 
                        Just sid -> redirectTo $ EditPartnerAction sid
                    HistorytypeTariff -> case getStateIdMB tariff wfp of
                        Nothing -> redirectTo NewTariffAction 
                        Just sid -> redirectTo $ EditTariffAction sid
                WftypeUpdate -> redirectUpdateState workflow
            Nothing -> do
                setErrorMessage $ "SHOULDN'T: progress is null workflowId= " ++ show workflowId
                redirectTo $ ShowWorkflowAction workflowId

    action CommitWorkflowAction = do
        workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        Log.info $ "ToCOmmitWF wf="++ show workflowId
        result <- case get #historyType  workflow of
            HistorytypeContract -> commitState contract workflow
            HistorytypePartner -> commitState contract workflow
            HistorytypeTariff -> commitState contract workflow
        case result of
            Left msg -> setSuccessMessage msg
            Right msg -> setErrorMessage msg
        redirectTo $ ShowWorkflowAction workflowId



    action RollbackWorkflowAction = do
        workflowId <- getCurrentWorkflowId
        putStrLn ("ToRollbackWF wf="++ show workflowId)
        redirectTo $ ShowWorkflowAction workflowId

    action SuspendWorkflowAction = do
        workflowId <- getCurrentWorkflowId
        putStrLn ("ToSuspendWF wf="++ show workflowId)
        redirectTo $ ShowWorkflowAction workflowId

    action ResumeWorkflowAction = do
        workflowId <- getCurrentWorkflowId
        setSession "workflowId" $ show workflowId
        putStrLn ("ToResumeWF wf="++ show workflowId)
        redirectTo $ ShowWorkflowAction workflowId

buildWorkflow workflow = workflow
    |> fill @["refUser","historyType","workflowType","validfrom"] |> set #progress val
        where val =fromJust $ decode $ fromStrict $ param @Web.Controller.Prelude.ByteString "progress"



redirectUpdateState :: (?context::ControllerContext, ?modelContext::ModelContext) => Workflow -> IO ()
redirectUpdateState workflow = do
    let histoType = get #historyType workflow
    updateState <- case histoType of
            HistorytypeContract -> getOrCreateStateIdForUpdate contract workflow
            HistorytypePartner -> getOrCreateStateIdForUpdate contract workflow
            HistorytypeTariff -> getOrCreateStateIdForUpdate contract workflow
    case updateState of
        Left (msg, key) -> do
            setSuccessMessage msg
            redirectEditState histoType key
        Right msg -> do 
            setErrorMessage "SHOULDN'T: history and state ids are null"
            redirectTo WorkflowsAction

getOrCreateStateIdForUpdate :: (?context::ControllerContext, ?modelContext::ModelContext, CanVersion rec) => (WorkflowProgress ->  Maybe (StateKeys (Id rec))) ->  Workflow -> IO (Either (Text,Integer) Text)
getOrCreateStateIdForUpdate accessor workflow = do
    case getStateIdMB accessor $ fromJust $ getWfp workflow of
        Nothing -> do
            mutable :: (rec,[Version]) <- queryMutableState accessor workflow
            let sid = getKey $ fst mutable
                msg :: Text = case snd mutable of
                            [] -> "not retrospective"
                            shadowed -> "the following versions will be shadowed: " ++ foldr ((++) . show . get #validfrom ) "" shadowed
            pure $ Left (msg, sid)
        Just sid -> pure $ Left ( "Version exists"::Text, fromId sid)
        
    

redirectEditState :: (?context::ControllerContext) => HistoryType -> Integer -> IO ()
redirectEditState HistorytypeContract sid = redirectTo $ EditContractAction $ Id sid
redirectEditState HistorytypePartner sid = redirectTo $ EditPartnerAction $ Id sid
redirectEditState HistorytypeTariff sid = redirectTo $ EditTariffAction $ Id sid