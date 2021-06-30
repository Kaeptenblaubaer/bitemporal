module Web.View.Contracts.Edit where
import Web.View.Prelude
import Web.Controller.Workflows

data EditView = EditView { workflowId :: Id Workflow, contractUpd :: ContractState }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">Edit ContractState</li>
            </ol>
        </nav>
        <h1>Edit ContractState</h1>
        {renderForm contractUpd}
    |]

renderForm :: ContractState -> Html
renderForm contract = formFor contract [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refEntity)}
    {(textField #content)}
    {submitButton}
|]
