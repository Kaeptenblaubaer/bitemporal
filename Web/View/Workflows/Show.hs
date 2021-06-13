module Web.View.Workflows.Show where
import Web.View.Prelude
import IHP.ControllerPrelude 
import Data.Text(replace, stripPrefix)
import Data.Maybe ( fromJust )

data ShowView = ShowView { workflow :: Workflow, stateHistoryIdMB :: Maybe UUID, stateVersionIdMB :: Maybe Integer , stateIdMB :: Maybe Integer}

stateHrefMB :: Maybe Integer -> HistoryType -> Html
stateHrefMB stateIdMB histoType = case stateIdMB of 
  Nothing -> [hsx| <li class="breadcrumb-item"> No state here </li> |]
  Just stateId -> [hsx| <li class="breadcrumb-item"><a href={ShowContractAction (Id stateId)}>{histoType}</a></li> |]


historyHrefMB :: Maybe UUID -> Html
historyHrefMB historyIdMB = case historyIdMB of 
  Nothing -> [hsx| <li class="breadcrumb-item"> No history here</li> |]
  Just historyId -> [hsx| <li class="breadcrumb-item"><a href={ShowHistoryAction (Id historyId)}>History</a></li> |]


instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkflowsAction}>Workflows</a></li>
                { historyHrefMB stateHistoryIdMB}                 
                { stateHrefMB stateIdMB $ get #historyType workflow }                 
                <li class="breadcrumb-item active">Show Workflow</li>
            </ol>
        </nav> 
        <h1>Show Workflow</h1>
        <p>{workflow}</p>
        <style type="text/css">
            jsoneditor {
              width: 500px;
              height: 500px;   
            }
        </style>
        <div id="jsoneditor"></div> 
        <table class="table"><thead>
          <tr><td>Commands</td></tr>
        </thead>
        <tbody>
          <tr>
            <td><a href={CommitWorkflowAction}>Commit</a></td>
            <td><a href={RollbackWorkflowAction}>Rollback</a></td>
            <td><a href={SuspendWorkflowAction}>Suspend</a></td>
            <td><a href={ResumeWorkflowAction}>Resume</a></td>
          </tr>
          </tbody></table>
        <script data-api-key= {show $ encode $ get #progress workflow }>
          var progress = document.currentScript.dataset.apiKey;
          progress = progress.substring(1,progress.length-1)
          // create the editor
          const container = document.getElementById('jsoneditor')
          const options = {}
          const editor = new JSONEditor(container, options)
          go()
  
          function go () {
            editor.set(JSON.parse(progress.replace(/\\/g,"")))
          }

        </script>
    |]