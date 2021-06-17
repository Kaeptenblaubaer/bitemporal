module Web.Controller.Partners where

import Web.Controller.Prelude
import Web.View.Partners.Index
import Web.View.Partners.New
import Web.View.Partners.Edit
import Web.View.Partners.Show

import Application.Helper.CanVersion;
import Data.Maybe
import qualified IHP.Log as Log
import IHP.Log.Types

instance Controller PartnersController where

    action PartnersAction = do
        workflow::Workflow <- getCurrentWorkflow
        partners <- query @Partner |> fetch
        render IndexView { .. }
        render IndexView { .. }

    action NewPartnerAction = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        let partnerNew :: Partner = newRecord
        render NewView { .. }

    action ShowPartnerAction { partnerId } = do
        workflowId <- getCurrentWorkflowId
        partner <- fetch partnerId
        render ShowView { .. }

    action EditPartnerAction { partnerId } = do
        workflow::Workflow <- getCurrentWorkflow
        let workflowId = get #id workflow
        partnerUpd <- fetch partnerId
        render EditView { workflowId, .. }

    action UpdatePartnerAction { partnerId } = do
        Log.info ("UpdateContractAction" ::String)
        workflowId <- getCurrentWorkflowId
        workflow <- fetch workflowId 
        partnerUpd <- fetch partnerId
        partnerUpd
            |> buildPartner
            |> ifValid \case
                Left partnerUpd -> do
                    Log.info ("UpdatePartner Error"::String)
                    render EditView { .. }
                Right partnerUpd -> do
                    Log.info ("UpdatePartner Success"::String)
                    partnerPers <- mutateHistory partner workflow partnerUpd
                    setSuccessMessage "Partner updated"
                    let partnerId = get #id partnerPers
                    redirectTo EditPartnerAction { .. }

    action CreatePartnerAction = do
        Log.info  ("Enter createHistory workflow=" :: String)
        workflowId <- getCurrentWorkflowId
        workflow :: Workflow <- fetch workflowId
        let partnerNew = newRecord @Partner
        partnerNew
            |> buildPartner
            |> ifValid \case
                Left partnerNew -> do
                    Log.info ("CreatePartner Error"::String)
                    render NewView { .. } 
                Right partnerNew -> do
                    
                    Log.info ("CreateContract Success"::String)
                    partnerCreated :: Partner <- createHistory partner workflow partnerNew
                    setSuccessMessage "Partner created"
                    let partnerId = get #id partnerCreated
                    redirectTo PartnersAction

    action DeletePartnerAction { partnerId } = do
        workflow::Workflow <- getCurrentWorkflow
        partner <- fetch partnerId
        deleteRecord partner
        setSuccessMessage "Partner deleted"
        redirectTo PartnersAction

buildPartner partner = partner
    |> fill @'["refHistory","refValidfromversion","refValidthruversion","content"]
