module Web.View.Histories.Show where
import Web.View.Prelude

data ShowView = ShowView { history :: Include "versions" History }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={HistoriesAction}>Histories</a></li>
                <li class="breadcrumb-item active">Show History</li>
            </ol>
        </nav>
        <h1>Show History</h1>
            <table class="table">
                <thead>
                    <tr>
                        <th>ID</th>
                        <th>HistoryType</th>
                    </tr>
                    </thead>
                    <tr>
                        <td>{get #id history}</td><td>{get #historyType history}</td>
                    </tr>   
                <tbody>
                    <table class="table">
                        <thead>
                        <tr>
                            <th><h2>Versions</h2></th>
                        </tr>
                            <tr>
                                <th>ID</th>
                                <th>validfrom</th>
                                <th>Command</th>
                            </tr>
                        </thead>
                        <tbody><div>{forEach (get #versions history) (renderVersion history)}</div></tbody>
                    </table>
                </tbody>
            </table>
        
    |]

renderVersion history version = [hsx|
    <tr>
        <td>{get #id version}</td><td>{get #validfrom version}</td><td><a href={pathTo(NewWorkflowAction) <> "?historyId=" ++ (show (get #id history))}>Start Mutation Workflow</a></td>
    </tr>
|]
