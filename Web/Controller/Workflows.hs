module Web.Controller.Workflows where

import Web.Controller.Prelude
import Web.View.Workflows.Index
import Web.View.Workflows.New
import Web.View.Workflows.Edit
import Web.View.Workflows.Show
import Application.Helper.Controller (createHistory, getKey, queryMutableState, today )
import Application.Helper.WorkflowProgress
import Data.Maybe ( fromJust )

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
                let initialWfpV :: Value = fromJust $ decode $ encode $ WorkflowProgress ( Just(StateKeys (Just historyId) Nothing Nothing )) Nothing
                workflow <- newRecord |> set #refuser (get #id user) |> set #validfrom today |>
                     set #workflowType WftypeUpdate |> set #historyType HistorytypeContract |> set #progress initialWfpV |> createRecord
                setCurrentWorkflowId workflow
                setModal NewView { .. }
            Nothing -> do
                let workflow = newRecord |> set #refuser (get #id user) |> set #validfrom today |>
                        set #workflowType WftypeNew 
                setModal NewView { .. }
        jumpToAction WorkflowsAction

    action ShowWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
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
        case get #historyType workflow of
            HistorytypeContract -> do
                case wfpMB of 
                    Just wfp -> case contract wfp of
                        Just c  -> case history c of
                            Just h -> do
                                    putStrLn "vor EditContract History==h"
                                    redirectTo EditContractAction
                            Nothing -> do
                                    setErrorMessage ("SHOULDN'T: history is null")
                                    redirectTo $ ShowWorkflowAction workflowId 
                        Nothing -> do
                                    setErrorMessage ("SHOULDN'T: contract is null")
                                    redirectTo NewContractAction 
                    Nothing -> case get #workflowType workflow of
                                WftypeNew -> do
                                    putStrLn "vor NewContract wfp == Nothing"
                                    redirectTo NewContractAction
                                wftype -> do
                                    setErrorMessage ("SHOULDN'T: " ++ (show wftype) ++ " with empty progress data")
                                    redirectTo NewContractAction 
            entitytype -> do
                setErrorMessage ((show entitytype) ++ "not implemented")
                redirectTo WorkflowsAction

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
                                        setSuccessMessage $ "version=" ++ show v
                                        v :: Version <- fetch (Id v) 
                                        v2 <- v |> set #committed True |> updateRecord
                                        w <- workflow |> set #workflowStatus "committed" |> updateRecord
                                        putStrLn "huhu"
                                        redirectTo $ ShowWorkflowAction workflowId 
                                    Nothing -> do
                                        setErrorMessage $ "No version history=" ++ show h
                                        redirectTo $ ShowWorkflowAction workflowId 
                            Nothing -> do
                                    setErrorMessage ("SHOULDN'T: history is null")
                                    redirectTo $ ShowWorkflowAction workflowId 
                        Nothing -> do
                                    setErrorMessage ("SHOULDN'T: contract is null")
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
        putStrLn ("ToResumeWF wf="++ (show workflowId))
        redirectTo $ ShowWorkflowAction workflowId

buildWorkflow workflow = workflow
    |> fill @["refuser","historyType","workflowType","validfrom"]
