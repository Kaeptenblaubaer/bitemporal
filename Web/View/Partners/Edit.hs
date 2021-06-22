module Web.View.Partners.Edit where
import Web.View.Prelude
import Web.Controller.Workflows

data EditView = EditView { workflowId :: Id Workflow, partnerUpd :: PartnerState }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={PartnersAction}>Partners</a></li>
                <li class="breadcrumb-item active">Edit PartnerState</li>
            </ol>
        </nav>
        <h1>Edit PartnerState</h1>
        {renderForm partnerUpd}
    |]

renderForm :: PartnerState -> Html
renderForm partner = formFor partner [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content)}
    {submitButton}
|]
