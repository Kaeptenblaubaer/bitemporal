module Web.View.Roles.Index where
import Web.View.Prelude

data IndexView = IndexView { roles :: [Role] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={RolesAction}>Roles</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewRoleAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Role</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach roles renderRole}</tbody>
            </table>
        </div>
    |]


renderRole role = [hsx|
    <tr>
        <td>{role}</td>
        <td><a href={ShowRoleAction (get #id role)}>Show</a></td>
        <td><a href={EditRoleAction (get #id role)} class="text-muted">Edit</a></td>
        <td><a href={DeleteRoleAction (get #id role)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
