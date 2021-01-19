module Web.View.Contracts.Edit where
import Web.View.Prelude
import Web.Controller.Workflows

data EditView = EditView { workflowId :: Id Workflow, contract :: Contract }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ShowWorkflowAction workflowId}>Workflow</a></li>
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">Edit Contract</li>
            </ol>
        </nav>
        <h1>Edit Contract</h1>
        {renderForm contract}
    |]

renderForm :: Contract -> Html
renderForm contract = formFor contract [hsx|
    {(textField #validfromversion)}
    {(textField #validthruversion)}
    {(textField #refhistory)}
    {(textField #content)}
    {submitButton}
|]
