module Web.View.Contracts.New where
import Web.View.Prelude
data NewView = NewView { workflowId :: Id Workflow, contractNew :: ContractState }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">New ContractState</li>
            </ol>
        </nav>
        <h1>New ContractState</h1>
        {renderForm contractNew }
 
    |]

renderForm :: ContractState ->Html
renderForm contract = formFor contract [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content ) }
    {submitButton}
|] 
