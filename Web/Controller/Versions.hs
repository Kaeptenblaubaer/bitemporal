module Web.Controller.Versions where

import Web.Controller.Prelude
import Web.View.Versions.Index
import Web.View.Versions.New
import Web.View.Versions.Edit
import Web.View.Versions.Show

instance Controller VersionsController where
    action VersionsAction = do
        versions <- query @Version |> fetch
        render IndexView { .. }

    action NewVersionAction = do
        let version = newRecord
        render NewView { .. }

    action ShowVersionAction { versionId } = do
        version <- fetch versionId
        render ShowView { .. }

    action EditVersionAction { versionId } = do
        version <- fetch versionId
        render EditView { .. }

    action UpdateVersionAction { versionId } = do
        version <- fetch versionId
        version
            |> buildVersion
            |> ifValid \case
                Left version -> render EditView { .. }
                Right version -> do
                    version <- version |> updateRecord
                    setSuccessMessage "Version updated"
                    redirectTo EditVersionAction { .. }

    action CreateVersionAction = do
        let version = newRecord @Version
        version
            |> buildVersion
            |> ifValid \case
                Left version -> render NewView { .. } 
                Right version -> do
                    version <- version |> createRecord
                    setSuccessMessage "Version created"
                    redirectTo VersionsAction

    action DeleteVersionAction { versionId } = do
        version <- fetch versionId
        deleteRecord version
        setSuccessMessage "Version deleted"
        redirectTo VersionsAction

    action ShowStateByVersionAction { versionId } = do
        version <- fetch versionId 
        history :: History <- fetch (get #refHistory version)
        case (get #historyType history) of
            HistorytypeContract -> do
                contractId :: Id ContractState <- queryImmutableState versionId
                redirectTo $ ShowContractAction contractId
            HistorytypePartner ->  do
                partnerId :: Id PartnerState <-  queryImmutableState versionId
                redirectTo $ ShowPartnerAction partnerId
            HistorytypeTariff ->    do
                tariffId :: Id TariffState <- queryImmutableState versionId
                redirectTo $ ShowTariffAction tariffId


buildVersion version = version
    |> fill @["refHistory","validfrom","committed","refShadowedby"]
