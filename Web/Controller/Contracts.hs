module Web.Controller.Contracts where

import Web.Controller.Prelude
import Web.View.Contracts.Index
import Web.View.Contracts.New
import Web.View.Contracts.Edit
import Web.View.Contracts.Show
import Application.Helper.Controller;
import Data.Maybe

today :: IO (Day) -- :: (year,month,day)
today = getCurrentTime >>= return . utctDay
maxday = fromGregorian 5874897 12 31


instance Controller ContractsController where

    action ContractsAction = do
        workflow::Workflow <- getCurrentWorkflow
        contracts <- query @Contract |> fetch
        render IndexView { .. }

    action NewContractAction = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        let contract ::Contract = newRecord 
        render NewView { .. }

    action ShowContractAction { contractId } = do
        workflowId <- getCurrentWorkflowId
        contract <- fetch contractId
        render ShowView { .. }

    action EditContractAction { contractId } = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        contract<- fetch contractId
        render EditView { workflowId, .. }

    action UpdateContractAction { contractId } = do 
        workflowId <- getCurrentWorkflowId
        workflow <- fetch workflowId
        contract <- fetch contractId
        contract
            |> buildContract
            |> ifValid \case
                Left contract -> render EditView { workflowId,  .. }
                Right contract -> do
                    contract <- mutateHistory workflow contract
                    setSuccessMessage "Contract updated"
                    let contractId = get #id contract
                    redirectTo EditContractAction {..}

    action CreateContractAction = do
        workflowId <- getCurrentWorkflowId
        workflow :: Workflow <- fetch workflowId
        let contract = newRecord @Contract
        contract
            |> buildContract
            |> ifValid \case
                Left contract -> render NewView { workflowId, .. } 
                Right contract -> do
                    contract :: Contract <- createHistory workflow contract
                    setSuccessMessage "Contract created"
                    redirectTo ContractsAction

    action DeleteContractAction { contractId } = do
        contract <- fetch contractId
        deleteRecord contract
        setSuccessMessage "Contract deleted"
        redirectTo ContractsAction

buildContract contract = contract
    |> fill @'[ "refHistory" ,"refValidfromversion" ,"refValidthruversion", "content"]