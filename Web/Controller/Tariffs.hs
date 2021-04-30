module Web.Controller.Tariffs where

import Web.Controller.Prelude
import Web.View.Tariffs.Index
import Web.View.Tariffs.New
import Web.View.Tariffs.Edit
import Web.View.Tariffs.Show
import Application.Helper.Controller;
import Data.Maybe
import qualified IHP.Log as Log
import IHP.Log.Types

instance Controller TariffsController where
    action TariffsAction = do
        workflow::Workflow <- getCurrentWorkflow
        tariffs <- query @Tariff |> fetch
        render IndexView { .. }

    action NewTariffAction = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        let tariff = newRecord
        render NewView { .. }

    action ShowTariffAction { tariffId } = do
        workflow::Workflow <- getCurrentWorkflow
        tariff <- fetch tariffId
        render ShowView { .. }

    action EditTariffAction { tariffId } = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        tariff <- fetch tariffId
        render EditView { .. }

    action UpdateTariffAction { tariffId } = do
        workflowId <- getCurrentWorkflowId
        workflow <- fetch workflowId 
        tariff <- fetch tariffId
        tariff
            |> buildTariff
            |> ifValid \case
                Left tariff -> render EditView { workflowId, .. }
                Right tariff -> do
                    tariff <- tariff |> updateRecord
                    setSuccessMessage "Tariff updated"
                    let tariffId = get #id tariff
                    redirectTo EditTariffAction { .. }

    action CreateTariffAction = do
        Log.info  "Enter createHistory workflow=" 
        workflowId <- getCurrentWorkflowId
        workflow :: Workflow <- fetch workflowId
        let tariff = newRecord @Tariff
        tariff
            |> buildTariff
            |> ifValid \case
                Left tariff -> render NewView { .. } 
                Right tariff -> do
                    tariff :: Tariff <- createHistory workflow tariff 
                    setSuccessMessage "Tariff created"
                    let tariffId = get #id tariff
                    redirectTo EditTariffAction {..}

    action DeleteTariffAction { tariffId } = do
        workflow::Workflow <- getCurrentWorkflow
        tariff <- fetch tariffId
        deleteRecord tariff
        setSuccessMessage "Tariff deleted"
        redirectTo TariffsAction

buildTariff tariff = tariff
    |> fill @'["refHistory","refValidfromversion","refValidthruversion","content"]