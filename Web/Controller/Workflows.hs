module Web.Controller.Workflows where

import Web.Controller.Prelude
import Web.View.Workflows.Index
import Web.View.Workflows.New
import Web.View.Workflows.Edit
import Web.View.Workflows.Show
import Web.Controller.Histories
import Application.Helper.Controller (createHistory, getKey, queryMutableState, today )
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
                Log.info "New Creation Workflow"
                let workflow = newRecord |> set #refUser (get #id user) |> set #validfrom today |>
                        set #workflowType WftypeNew
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
                            initialProgress :: Value = initialWfpV hType historyId
                            workflow = newRecord |> set #refUser (get #id user) |> set #validfrom today |> set #workflowType WftypeUpdate |>
                                set #historyType hType |> set #progress initialProgress
                        setModal NewView { .. }
        jumpToAction WorkflowsAction

    action ShowWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        setSession "workflowId" (show workflowId)
        Log.info $ "Show Workflow. Set Session workflowId= " ++ show workflowId
        let histoType = get #historyType workflow
        Log.info $ "HistoryType= " ++ show histoType
        let wfp = fromJust $ getWfp workflow
            stateHistoryIdMB = getStatehistoryIdMB wfp histoType
            stateVersionIdMB = getStateVersionIdMB wfp histoType
            stateIdMB = getStateIdMB wfp histoType
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
        Log.info $ "creating Workflow"
        let workflow = newRecord @Workflow
        workflow
            |> buildWorkflow
            |> ifValid \case
                Left workflow -> render NewView { .. }
                Right workflow -> do
                    workflow <- workflow |> createRecord 
                    workflow <- workflow |> set #progress (fromJust $ decode $ encode $ WorkflowProgress Nothing Nothing Nothing ) |> updateRecord 
                    setCurrentWorkflowId workflow
                    Log.info $ "wfnew sessionset =" ++ show workflow
                    setSuccessMessage "Workflow created"
                    Log.info $ "redirecting to NextWorkflowAction " ++ (show $ get #id workflow)
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
                WftypeNew -> redirectCreateState (get #historyType workflow) $getStateIdMB wfp HistorytypeContract
                WftypeUpdate -> redirectUpdateState workflow
            Nothing -> do
                setErrorMessage $ "SHOULDN'T: progress is null workflowId= " ++ show workflowId
                redirectTo $ ShowWorkflowAction workflowId

    action CommitWorkflowAction = do
        workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        Log.info $ "ToCOmmitWF wf="++ show workflowId
        let wfpMB = getWfp workflow
            histoType = get #historyType workflow
        case wfpMB of
            Just wfp -> do
                case getStatehistoryIdMB wfp histoType of
                    Just h -> case getStateVersionIdMB wfp histoType of
                        Just v -> case getStateIdMB wfp histoType of
                            Just s -> withTransaction do 
                                Log.info $ "committing h:" ++ show h
                                hUnlocked :: History <- fetch (Id h)
                                hUnlocked |> set #refOwnedByWorkflow Nothing |> updateRecord
                                Log.info $ "Unlocked h:" ++ show h
                                setSuccessMessage $ "version=" ++ show v
                                newVersion :: Version <- fetch (Id v)
                                newVersion |>set #committed True |> updateRecord
                                Log.info $ "commit version v: " ++ show v
                                w <- workflow |> set #workflowStatus "committed" |> updateRecord
                                Log.info $ "commit workflow w: " ++ show w
                                sOld :: [Contract] <- query @ Contract |> filterWhere (#refHistory,Id h) |>
                                    filterWhereSql(#refValidfromversion,"<> " ++ encodeUtf8( show v)) |>
                                    filterWhere(#refValidthruversion,Nothing) |> fetch
                                case head sOld of
                                    Just sOld -> do
                                            sUpd :: Contract <- sOld |> set #refValidthruversion (Just (Id v)) |> updateRecord
                                            Log.info $ "contract predecessor terminated" ++ show s
                                    Nothing -> Log.info "no contract predecessor"
                                case getShadowed wfp of
                                    Nothing -> Log.info "No version"
                                    Just (shadow,shadowed) -> do
                                        updated :: [Version]<- sqlQuery "update versions v set ref_shadowedby = ? where id in ? returning * " (v, In shadowed)
                                        forEach updated (\v -> Log.info $ "updated" ++ show v)
                                commitTransaction
                                Log.info "commit successful"
                                redirectTo $ ShowWorkflowAction workflowId
                            Nothing -> do
                                setErrorMessage $ "cannot commit: state is null h=" ++ show h ++ "v=" ++ show v
                                redirectTo $ ShowWorkflowAction workflowId
                        Nothing -> do
                            setErrorMessage $ "cannot commit: version is null h=" ++ show h
                            redirectTo $ ShowWorkflowAction workflowId
                    Nothing -> do
                        setErrorMessage "cannot commit: history is null"
                        redirectTo $ ShowWorkflowAction workflowId
            Nothing -> do
                setErrorMessage "SHOULDN'T: empty progress data"
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

redirectCreateState :: (?context::ControllerContext) => HistoryType -> Maybe Integer -> IO ()
redirectCreateState HistorytypeContract Nothing  = redirectTo NewContractAction 
redirectCreateState HistorytypeContract (Just sid)  = redirectTo $ EditContractAction $ Id sid
redirectCreateState HistorytypePartner Nothing  = redirectTo NewPartnerAction 
redirectCreateState HistorytypePartner (Just sid)  = redirectTo $ EditPartnerAction $ Id sid
redirectCreateState HistorytypeTariff Nothing  = redirectTo NewTariffAction 
redirectCreateState HistorytypeTariff (Just sid)  = redirectTo $ EditTariffAction $ Id sid

queryMutableStateKey :: (?context::context, ?modelContext::ModelContext, LoggingProvider context) => Workflow' (Id' "users") (QueryBuilder "histories") -> IO (Integer, [Version])
queryMutableStateKey workflow =  do
    case get #historyType workflow of
        HistorytypeContract -> do 
                mutable :: (Contract,[Version]) <- queryMutableState workflow
                pure ( (getKey $ fst mutable, snd mutable))
        HistorytypePartner -> do 
                mutable :: (Partner,[Version]) <- queryMutableState workflow
                pure ( (getKey $ fst mutable, snd mutable))
        HistorytypeTariff -> do 
                mutable :: (Tariff,[Version]) <- queryMutableState workflow
                pure ( (getKey $ fst mutable, snd mutable))

redirectUpdateState :: (?context::ControllerContext, ?modelContext::ModelContext) => Workflow -> IO ()
redirectUpdateState workflow = do
    let histoType = get #historyType workflow 
    case getWorkFlowState (fromJust $ getWfp workflow) histoType of
            Just (StateKeys (Just hid) Nothing Nothing Nothing) -> do
                mutable :: (Integer,[Version]) <- queryMutableStateKey workflow
                let msg :: Text = case snd mutable of
                        [] -> "not retrospective"
                        shadowed -> "the following versions will be shadowed: " ++ foldr (((++)) . (\v -> show $ get #validfrom v)) "" shadowed
                setSuccessMessage msg
                redirectEditState histoType (fst mutable)
            Just (StateKeys _ _ (Just sid) _) ->
                redirectEditState histoType sid
            _ -> do 
                setErrorMessage "SHOULDN'T: history and state ids are null"
                redirectTo WorkflowsAction
        


redirectEditState :: (?context::ControllerContext) => HistoryType -> Integer -> IO ()
redirectEditState HistorytypeContract sid = redirectTo $ EditContractAction $ Id sid
redirectEditState HistorytypePartner sid = redirectTo $ EditPartnerAction $ Id sid
redirectEditState HistorytypeContract sid = redirectTo $ EditTariffAction $ Id sid