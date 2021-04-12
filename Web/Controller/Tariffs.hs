module Web.Controller.Tariffs where

import Web.Controller.Prelude
import Web.View.Tariffs.Index
import Web.View.Tariffs.New
import Web.View.Tariffs.Edit
import Web.View.Tariffs.Show

instance Controller TariffsController where
    action TariffsAction = do
        tariffs <- query @Tariff |> fetch
        render IndexView { .. }

    action NewTariffAction = do
        let tariff = newRecord
        render NewView { .. }

    action ShowTariffAction { tariffId } = do
        tariff <- fetch tariffId
        render ShowView { .. }

    action EditTariffAction { tariffId } = do
        tariff <- fetch tariffId
        render EditView { .. }

    action UpdateTariffAction { tariffId } = do
        tariff <- fetch tariffId
        tariff
            |> buildTariff
            |> ifValid \case
                Left tariff -> render EditView { .. }
                Right tariff -> do
                    tariff <- tariff |> updateRecord
                    setSuccessMessage "Tariff updated"
                    redirectTo EditTariffAction { .. }

    action CreateTariffAction = do
        let tariff = newRecord @Tariff
        tariff
            |> buildTariff
            |> ifValid \case
                Left tariff -> render NewView { .. } 
                Right tariff -> do
                    tariff <- tariff |> createRecord
                    setSuccessMessage "Tariff created"
                    redirectTo TariffsAction

    action DeleteTariffAction { tariffId } = do
        tariff <- fetch tariffId
        deleteRecord tariff
        setSuccessMessage "Tariff deleted"
        redirectTo TariffsAction

buildTariff tariff = tariff
    |> fill @["refValidfromversion","refValidthruversion","refHistory","content"]
