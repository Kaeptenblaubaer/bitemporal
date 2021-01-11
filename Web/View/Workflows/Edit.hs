module Web.View.Workflows.Edit where
import Web.View.Prelude

data EditView = EditView { workflow :: Workflow }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkflowsAction}>Workflows</a></li>
                <li class="breadcrumb-item active">Edit Workflow</li>
            </ol>
        </nav>
        <h1>Edit Workflow</h1>
        {renderForm workflow}
    |]

renderForm :: Workflow -> Html
renderForm workflow = formFor workflow [hsx|
    {(textField #refuser)}
    {(textField #historyType)}
    {(textField #workflowType)}
    {(textField #progress)}
    {submitButton}
|]
