module Web.View.Workflows.Show where
import Web.View.Prelude
import IHP.ControllerPrelude 
import Data.Text(replace, stripPrefix)

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
        <style type="text/css">
            #jsoneditor {
              width: 500px;
              height: 500px;   
            }
        </style>
        <div id="jsoneditor"></div>
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