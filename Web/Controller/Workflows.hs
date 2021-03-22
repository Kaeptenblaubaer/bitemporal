module Web.Controller.Workflows where

import Web.Controller.Prelude
import Web.View.Workflows.Index
import Web.View.Workflows.New
import Web.View.Workflows.Edit
import Web.View.Workflows.Show
import Web.Controller.Histories
import Application.Helper.Controller (createHistory, getKey, queryMutableState, today )
import Application.Helper.WorkflowProgress
import Data.Maybe ( fromJust )
import Data.ByteString.Lazy as BSL (ByteString,fromStrict)
import Data.Text.Encoding(encodeUtf8)
import IHP.Log as Log

instance Controller WorkflowsController where
    action WorkflowsAction = do
        let validfrom = paramOrNothing @Day "validfrom"
        workflows <- query @Workflow |> fetch
        render IndexView { .. }

    action NewWorkflowAction = do  
        today <- today
        now <- getCurrentTime
        user <- query @User |> fetchOne 
        let historyIdMB = paramOrNothing @UUID "historyId"
        case historyIdMB of 
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
                        let initialWfpV :: Value = fromJust $ decode $ encode $ WorkflowProgress (Just $ StateKeys (Just historyId) Nothing Nothing Nothing) Nothing
                        let workflow = newRecord |> set #refUser (get #id user) |> set #validfrom today |> set #workflowType WftypeUpdate |> set #historyType HistorytypeContract |> set #progress initialWfpV 
                        setModal NewView { .. }
            Nothing -> do
                Log.info $ "New Creation Workflow"
                let workflow = newRecord |> set #refUser (get #id user) |> set #validfrom today |>
                        set #workflowType WftypeNew 
                setModal NewView { .. }
        jumpToAction WorkflowsAction

    action ShowWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        setSession "workflowId" (show workflowId)
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
        let workflow = newRecord @Workflow
        workflow
            |> buildWorkflow
            |> ifValid \case 
                Left workflow -> render NewView { .. } 
                Right workflow -> do
                    workflow <- workflow |> createRecord
                    setCurrentWorkflowId workflow
                    putStrLn ("wfnew sessionset =" ++ (show workflow)) 
                    setSuccessMessage "Workflow created"
                    redirectTo WorkflowsAction

    action DeleteWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        deleteRecord workflow
        setSuccessMessage "Workflow deleted"
        redirectTo WorkflowsAction
        
    action NextWorkflowAction { workflowId } = do
        workflow <- getCurrentWorkflow
        putStrLn ("NextWF wf="++ (show workflow))
        let wfpMB = getWfp workflow
        case get #workflowType workflow of
            WftypeNew -> case wfpMB of 
                Just wfp -> case contract wfp of
                    Just (StateKeys _ _ (Just rid) _) -> do
                        redirectTo $ EditContractAction (Id rid)
                    _ -> do
                        setErrorMessage ("SHOULDN'T: history is null")
                        redirectTo $ ShowWorkflowAction workflowId 
                Nothing -> do
                        redirectTo NewContractAction 
            WftypeUpdate -> case wfpMB of 
                Just wfp -> case contract wfp of
                    Just (StateKeys (Just hid) Nothing Nothing Nothing) -> do
                        mutable :: (Contract,[Version]) <- queryMutableState workflow
                        let msg :: Text = case (snd mutable) of
                                [] -> "not retrospective"
                                shadowed -> "the following versions will be shadowed: " ++ foldr  (++) ""  (map (\v -> show $ get #validfrom v) shadowed)
                        setSuccessMessage msg
                        redirectTo $ EditContractAction (get #id (fst mutable))
                    Just (StateKeys _ _ (Just rid) _) -> do
                        redirectTo $ EditContractAction (Id rid)
                    Nothing -> do
                        redirectTo NewContractAction
                    _ -> do
                        setErrorMessage ("SHOULDN'T: history is null")
                        redirectTo $ ShowWorkflowAction workflowId 
                Nothing -> do
                        setErrorMessage ("SHOULDN'T: wfp is null")
                        redirectTo $ ShowWorkflowAction workflowId 

       
    action CommitWorkflowAction = do
        workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        putStrLn ("ToCOmmitWF wf="++ (show workflowId))
        let wfpMB = getWfp workflow
        case get #historyType workflow of
            HistorytypeContract -> do
                case wfpMB of 
                    Just wfp -> case contract wfp of
                        Just c  -> case history c of
                            Just h -> do
                                case version c of
                                    Just v -> do
                                        case state c of
                                            Just s -> withTransaction do
                                                hUnlocked :: History <- fetch (Id h)
                                                hUnlocked |> set #refOwnedByWorkflow Nothing |> updateRecord 
                                                setSuccessMessage $ "version=" ++ show v
                                                newVersion :: Version <- fetch (Id v) 
                                                newVersion |>set #committed True |> updateRecord 
                                                w <- workflow |> set #workflowStatus "committed" |> updateRecord
                                                sOld :: [Contract] <- query @ Contract |> filterWhere (#refHistory,(Id h)) |> 
                                                    filterWhereSql(#refValidfromversion,"<> " ++ encodeUtf8( show v)) |>
                                                    filterWhere(#refValidthruversion,Nothing) |> fetch
                                                case head sOld of
                                                    Just sOld -> do
                                                            sUpd :: Contract <- sOld |> set #refValidthruversion (Just (Id v)) |> updateRecord
                                                            putStrLn "predecessor terminated"
                                                    Nothing -> putStrLn "no predecessor"
                                                case getShadowed wfp of
                                                    Nothing -> putStrLn "none shadowed"
                                                    Just (shadow,shadowed) -> do
                                                        updated :: [Version]<- sqlQuery "update version set refshadowed = ? where id in ?" (shadow, In shadowed)
                                                        putStrLn "updated"
                                                redirectTo $ ShowWorkflowAction workflowId 
                                            Nothing -> do
                                                setErrorMessage $ "cannot commit: state is null h=" ++ show h ++ "v=" ++ show v
                                                redirectTo $ ShowWorkflowAction workflowId 
                                    Nothing -> do
                                        setErrorMessage $ "cannot commit: version is null h=" ++ show h
                                        redirectTo $ ShowWorkflowAction workflowId 
                            Nothing -> do
                                    setErrorMessage ("cannot commit: history is null")
                                    redirectTo $ ShowWorkflowAction workflowId 
                        Nothing -> do
                                    setErrorMessage ("cannot commit, SHOULDN'T: contract is null")
                                    redirectTo $ ShowWorkflowAction workflowId 
                    Nothing -> do
                            setErrorMessage "SHOULDN'T: empty progress data"
                            redirectTo $ ShowWorkflowAction workflowId 
            entitytype -> do
                setErrorMessage ((show entitytype) ++ "not implemented")
                redirectTo $ ShowWorkflowAction workflowId 

    action RollbackWorkflowAction = do
        workflowId <- getCurrentWorkflowId
        putStrLn ("ToRollbackWF wf="++ (show workflowId))
        redirectTo $ ShowWorkflowAction workflowId

    action SuspendWorkflowAction = do
        workflowId <- getCurrentWorkflowId
        putStrLn ("ToSuspendWF wf="++ (show workflowId))
        redirectTo $ ShowWorkflowAction workflowId

    action ResumeWorkflowAction = do
        workflowId <- getCurrentWorkflowId
        setSession "workflowId" $ show workflowId
        putStrLn ("ToResumeWF wf="++ (show workflowId))
        redirectTo $ ShowWorkflowAction workflowId

buildWorkflow workflow = workflow
    |> fill @["refUser","historyType","workflowType","validfrom"] |> set #progress val
        where val =fromJust $ decode $ fromStrict $ param @Web.Controller.Prelude.ByteString "progress"