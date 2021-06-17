module Web.View.Partners.Edit where
import Web.View.Prelude
import Web.Controller.Workflows

data EditView = EditView { workflowId :: Id Workflow, partnerUpd :: Partner }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={PartnersAction}>Partners</a></li>
                <li class="breadcrumb-item active">Edit Partner</li>
            </ol>
        </nav>
        <h1>Edit Partner</h1>
        {renderForm partnerUpd}
    |]

renderForm :: Partner -> Html
renderForm partner = formFor partner [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content)}
    {submitButton}
|]
