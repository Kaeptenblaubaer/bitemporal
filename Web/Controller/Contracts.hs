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
        let workflowId = param @(Id Workflow) "workflowId"
        workflow::Workflow <- fetch workflowId
        contracts <- query @Contract |> fetch
        render IndexView { .. }

    action NewContractAction = do
        workflowIdMB <- getSessionUUID "workflowId"
        putStrLn (show workflowIdMB)
        workflow::Workflow <- fetch (Id (fromJust workflowIdMB))
        contract::Contract <- createHistory workflow
        render NewView { .. }

    action ShowContractAction { contractId } = do
        contract <- fetch contractId
        render ShowView { .. }

    action EditContractAction = do
        workflowIdMB <- getSessionUUID "workflowId"
        putStrLn (show workflowIdMB)
        workflow::Workflow <- fetch (Id (fromJust workflowIdMB))
        mutable :: (Contract,[Version]) <- queryMutableState workflow
        let contract = fst mutable
        render EditView { .. }

    action UpdateContractAction { contractId } = do
        contract <- fetch contractId
        contract
            |> buildContract
            |> ifValid \case
                Left contract -> render EditView { .. }
                Right contract -> do
                    contract <- contract |> updateRecord
                    setSuccessMessage "Contract updated"
                    redirectTo EditContractAction

    action CreateContractAction = do
        let contract = newRecord @Contract
        workflowIdMB <-getSessionUUID "workflowId"
        let workflowId = fromJust workflowIdMB
        workflow :: Workflow <- fetch (Id workflowId)
        putStrLn("####"++ (show workflow))
        contract
            |> buildContract
            |> ifValid \case
                Left contract -> render NewView { .. } 
                Right contract -> do
                    contract <- contract |> createRecord
                    setSuccessMessage "Contract created"
                    redirectToPath (pathTo ContractsAction <> "?workflowId= " ++ (show workflowId))

    action DeleteContractAction { contractId } = do
        contract <- fetch contractId
        deleteRecord contract
        setSuccessMessage "Contract deleted"
        redirectTo ContractsAction

buildContract contract = contract
    |> fill @'[ "refhistory" ,"validfromversion" ,"validthruversion", "content"]