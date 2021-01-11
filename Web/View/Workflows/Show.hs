module Web.View.Workflows.Show where
import Web.View.Prelude

data ShowView = ShowView { workflow :: Workflow }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkflowsAction}>Workflows</a></li>
                <li class="breadcrumb-item active">Show Workflow</li>
            </ol>
        </nav>
        <h1>Show Workflow</h1>
        <p>{workflow}</p>
    |]
