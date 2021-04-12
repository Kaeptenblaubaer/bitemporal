module Web.View.Versions.Index where
import Web.View.Prelude

data IndexView = IndexView { versions :: [Version] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={VersionsAction}>Versions</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewVersionAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Version</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach versions renderVersion}</tbody>
            </table>
        </div>
    |]


renderVersion version = [hsx|
    <tr>
        <td>{version}</td>
        <td><a href={ShowVersionAction (get #id version)}>Show</a></td>
        <td><a href={EditVersionAction (get #id version)} class="text-muted">Edit</a></td>
        <td><a href={DeleteVersionAction (get #id version)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
