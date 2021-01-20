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
        contract::Contract <- createHistory workflow
        render NewView { .. }

    action ShowContractAction { contractId } = do
        workflowId <- getCurrentWorkflowId
        contract <- fetch contractId
        render ShowView { .. }

    action EditContractAction = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        mutable :: (Contract,[Version]) <- queryMutableState workflow
        let msg :: Text = case (snd mutable) of
                    [] -> "not retrospective"
                    shadowed -> foldr  (++) "the following versions will be shadowed: "  (map (\v -> show $ get #validfrom v) shadowed)
        setSuccessMessage msg
        contract<- mutateHistory workflow $ fst mutable
        render EditView { workflowId, .. }

    action UpdateContractAction { contractId } = do
        workflowId <- getCurrentWorkflowId
        contract <- fetch contractId
        contract
            |> buildContract
            |> ifValid \case
                Left contract -> render EditView { workflowId,  .. }
                Right contract -> do
                    contract <- contract |> updateRecord
                    setSuccessMessage "Contract updated"
                    redirectTo EditContractAction

    action CreateContractAction = do
        workflowId <- getCurrentWorkflowId
        let contract = newRecord @Contract
        contract
            |> buildContract
            |> ifValid \case
                Left contract -> render NewView { workflowId, .. } 
                Right contract -> do
                    contract <- contract |> createRecord
                    setSuccessMessage "Contract created"
                    redirectTo ContractsAction

    action DeleteContractAction { contractId } = do
        contract <- fetch contractId
        deleteRecord contract
        setSuccessMessage "Contract deleted"
        redirectTo ContractsAction

buildContract contract = contract
    |> fill @'[ "refhistory" ,"validfromversion" ,"validthruversion", "content"]