module Web.View.Userroles.Index where
import Web.View.Prelude

data IndexView = IndexView { userroles :: [Userrole] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={UserrolesAction}>Userroles</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewUserroleAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Userrole</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach userroles renderUserrole}</tbody>
            </table>
        </div>
    |]


renderUserrole userrole = [hsx|
    <tr>
        <td>{userrole}</td>
        <td><a href={ShowUserroleAction (get #id userrole)}>Show</a></td>
        <td><a href={EditUserroleAction (get #id userrole)} class="text-muted">Edit</a></td>
        <td><a href={DeleteUserroleAction (get #id userrole)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
