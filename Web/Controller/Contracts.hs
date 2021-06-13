module Web.Controller.Contracts where

import Web.Controller.Prelude
import Web.View.Contracts.Index
import Web.View.Contracts.New
import Web.View.Contracts.Edit
import Web.View.Contracts.Show
import Application.Helper.CanVersion;
import Data.Maybe
import qualified IHP.Log as Log
import IHP.Log.Types

instance Controller ContractsController where

    action ContractsAction = do
        workflow::Workflow <- getCurrentWorkflow
        contracts <- query @Contract |> fetch
        render IndexView { .. }

    action NewContractAction = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        let contractNew ::Contract = newRecord 
        render NewView { .. }

    action ShowContractAction { contractId } = do
        workflowId <- getCurrentWorkflowId
        contract <- fetch contractId
        render ShowView { .. }

    action EditContractAction { contractId } = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        contractUpd<- fetch contractId
        render EditView { workflowId, .. }

    action UpdateContractAction { contractId } = do 
        workflowId <- getCurrentWorkflowId
        workflow <- fetch workflowId 
        contractUpd <- fetch contractId
        contractUpd
            |> buildContract
            |> ifValid \case
                Left contractUpd -> render EditView { workflowId,  .. }
                Right contractUpd -> do
                    contractPers <- mutateHistory contract workflow contractUpd
                    setSuccessMessage "Contract updated"
                    let contractId = get #id contractPers
                    redirectTo EditContractAction {..}

    action CreateContractAction = do
        Log.info  ("Enter createHistory workflow=" :: String)
        workflowId <- getCurrentWorkflowId
        workflow :: Workflow <- fetch workflowId
        let contractNew = newRecord @Contract
        contractNew
            |> buildContract
            |> ifValid \case
                Left contractNew -> render NewView { workflowId, .. } 
                Right contractNew -> do
                    contractCreated :: Contract <- createHistory contract workflow contractNew
                    setSuccessMessage "Contract created"
                    let contractId = get #id contractCreated
                    redirectTo EditContractAction {..}

    action DeleteContractAction { contractId } = do
        workflow::Workflow <- getCurrentWorkflow
        contract <- fetch contractId
        deleteRecord contract
        setSuccessMessage "Contract deleted"
        redirectTo ContractsAction

buildContract contract = contract
    |> fill @'["refHistory","refValidfromversion","refValidthruversion","content"]
