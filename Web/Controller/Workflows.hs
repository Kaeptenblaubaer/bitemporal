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
        putStrLn ("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@historyId=" ++ (show historyIdMB))
        case historyIdMB of 
            Just historyId ->  do
                let initialWfpV :: Value = fromJust $ decode $ encode $ WorkflowProgress ( Just(StateKeys (Just historyId) Nothing Nothing )) Nothing
                workflow <- newRecord |> set #refuser (get #id user) |> set #validfrom today |>
                     set #workflowType WftypeUpdate |> set #historyType HistorytypeContract |> set #progress initialWfpV |> createRecord
                putStrLn ("UpdateWF=" ++ show workflow)
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
                    setSession (show (get #id workflow)) "workflowId"
                    setSuccessMessage "Workflow created"
                    redirectTo WorkflowsAction

    action DeleteWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        deleteRecord workflow
        setSuccessMessage "Workflow deleted"
        redirectTo WorkflowsAction
        
    action NextWorkflowAction { workflowId } = do
        workflow <- fetch workflowId
        setSession "workflowId" (show(get #id workflow))
        let historyIdMB = paramOrNothing @(Id History) "historyId"
        putStrLn (show historyIdMB)
        case get #historyType workflow of
            HistorytypeContract -> 
                case get #workflowType workflow of
                    WftypeNew-> redirectTo NewContractAction
                    WftypeUpdate -> redirectTo(EditContractAction )
            entitytype -> do
                    setErrorMessage ((show entitytype) ++ "not implemented")
                    redirectTo WorkflowsAction

buildWorkflow workflow = workflow
    |> fill @["refuser","historyType","workflowType","validfrom"]
