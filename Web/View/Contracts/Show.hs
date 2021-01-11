module Web.View.Contracts.Show where
import Web.View.Prelude

data ShowView = ShowView { contract :: Contract }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">Show Contract</li>
            </ol>
        </nav>
        <h1>Show Contract</h1>
        <p>{contract}</p>
        <style type="text/css">
            #jsoneditor {
              width: 500px;
              height: 500px;   
            }
        </style>
        <p>
          <button id="setJSON">Set JSON</button>
          <button id="getJSON">Get JSON</button>
        </p>
        <div id="jsoneditor"></div>

        <script>
          // create the editor
          const container = document.getElementById('jsoneditor')
          const options = {}
          const editor = new JSONEditor(container, options)

          // set json
          document.getElementById('setJSON').onclick = function () {
            const json = {
              'array': [1, 2, 3],
              'boolean': true,
              'color': '#82b92c',
              'null': null,
              'number': 123,
              'object': {'a': 'b', 'c': 'd'},
              'time': 1575599819000,
              'string': 'Hello World',
              'onlineDemo': 'https://jsoneditoronline.org/'
            }
            editor.set(json)
          }

          // get json
          document.getElementById('getJSON').onclick = function () {
            const json = editor.get()
            alert(JSON.stringify(json, null, 2))
            window.location.href = "Contracts" + "?json="+ JSON.stringify(json, null)
          }
        </script>
    |]
