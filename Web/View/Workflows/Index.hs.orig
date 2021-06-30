module Web.View.Workflows.Index where
import Web.View.Prelude

data IndexView = IndexView { workflows :: [Workflow] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={WorkflowsAction}>Workflows</a></li>
            </ol>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={HistoriesAction}>Histories</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewWorkflowAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Workflow</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach workflows renderWorkflow}</tbody>
            </table>
        </div>
    |]


renderWorkflow workflow = [hsx|
    <tr>
        <td>{get #id workflow}</td>
        <td>{get #createdat workflow}</td><td>{get #validfrom workflow}</td><td>{get #workflowType workflow}</td><td>{get #historyType workflow}</td>
        <td><a href={NextWorkflowAction (get #id workflow)}>NextAction</a></td>
        <td><a href={ShowWorkflowAction (get #id workflow)}>Show</a></td>
        <td><a href={EditWorkflowAction (get #id workflow)} class="text-muted">Edit</a></td>
        <td><a href={DeleteWorkflowAction (get #id workflow)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
