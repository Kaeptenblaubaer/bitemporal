module Web.View.Contracts.New where
import Web.View.Prelude
data NewView = NewView { workflowId :: Id Workflow, contract :: Contract }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">New Contract</li>
            </ol>
        </nav>
        <h1>New Contract</h1>
        {renderForm contract }
 
    |]

renderForm :: Contract ->Html
renderForm contract = formFor contract [hsx|
    {(textField #refvalidfromversion)}
    {(textField #refvalidthruversion)}
    {(textField #refhistory)}
    {(textField #content ) }
    {submitButton}
|] 
