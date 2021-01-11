module Web.Controller.Partners where

import Web.Controller.Prelude
import Web.View.Partners.Index
import Web.View.Partners.New
import Web.View.Partners.Edit
import Web.View.Partners.Show

instance Controller PartnersController where
    action PartnersAction = do
        partners <- query @Partner |> fetch
        render IndexView { .. }

    action NewPartnerAction = do
        let partner = newRecord
        render NewView { .. }

    action ShowPartnerAction { partnerId } = do
        partner <- fetch partnerId
        render ShowView { .. }

    action EditPartnerAction { partnerId } = do
        partner <- fetch partnerId
        render EditView { .. }

    action UpdatePartnerAction { partnerId } = do
        partner <- fetch partnerId
        partner
            |> buildPartner
            |> ifValid \case
                Left partner -> render EditView { .. }
                Right partner -> do
                    partner <- partner |> updateRecord
                    setSuccessMessage "Partner updated"
                    redirectTo EditPartnerAction { .. }

    action CreatePartnerAction = do
        let partner = newRecord @Partner
        partner
            |> buildPartner
            |> ifValid \case
                Left partner -> render NewView { .. } 
                Right partner -> do
                    partner <- partner |> createRecord
                    setSuccessMessage "Partner created"
                    redirectTo PartnersAction

    action DeletePartnerAction { partnerId } = do
        partner <- fetch partnerId
        deleteRecord partner
        setSuccessMessage "Partner deleted"
        redirectTo PartnersAction

buildPartner partner = partner
    |> fill @["validfromversion","validthruversion","refhistory","content"]
