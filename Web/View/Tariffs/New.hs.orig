module Web.View.Tariffs.New where
import Web.View.Prelude

data NewView = NewView { workflowId :: Id Workflow, tariffNew :: Tariff }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={TariffsAction}>Tariffs</a></li>
                <li class="breadcrumb-item active">New Tariff</li>
            </ol>
        </nav>
        <h1>New Tariff</h1>
        {renderForm tariffNew}
    |]

renderForm :: Tariff -> Html
renderForm tariff = formFor tariff [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content)}
    {submitButton}
|]
