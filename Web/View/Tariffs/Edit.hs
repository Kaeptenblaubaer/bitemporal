module Web.View.Tariffs.Edit where
import Web.View.Prelude

data EditView = EditView { workflowId :: Id Workflow, tariff :: TariffState }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={TariffsAction}>Tariffs</a></li>
                <li class="breadcrumb-item active">Edit TariffState</li>
            </ol>
        </nav>
        <h1>Edit TariffState</h1>
        {renderForm tariff}
    |]

renderForm :: TariffState -> Html
renderForm tariff = formFor tariff [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content)}
    {submitButton}
|]
