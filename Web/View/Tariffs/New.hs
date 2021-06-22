module Web.View.Tariffs.New where
import Web.View.Prelude

data NewView = NewView { workflowId :: Id Workflow, tariffNew :: TariffState }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={TariffsAction}>Tariffs</a></li>
                <li class="breadcrumb-item active">New TariffState</li>
            </ol>
        </nav>
        <h1>New TariffState</h1>
        {renderForm tariffNew}
    |]

renderForm :: TariffState -> Html
renderForm tariff = formFor tariff [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content)}
    {submitButton}
|]
