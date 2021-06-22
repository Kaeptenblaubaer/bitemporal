module Web.View.Partners.New where
import Web.View.Prelude
data NewView = NewView { workflowId :: Id Workflow, partnerNew :: PartnerState }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={PartnersAction}>Partners</a></li>
                <li class="breadcrumb-item active">New PartnerState</li>
            </ol>
        </nav>
        <h1>New PartnerState</h1>
        {renderForm partnerNew}
    |]

renderForm :: PartnerState -> Html
renderForm partner = formFor partner [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content)}
    {submitButton}
|]
