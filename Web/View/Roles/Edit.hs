module Web.View.Roles.Edit where
import Web.View.Prelude

data EditView = EditView { role :: Role }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={RolesAction}>Roles</a></li>
                <li class="breadcrumb-item active">Edit Role</li>
            </ol>
        </nav>
        <h1>Edit Role</h1>
        {renderForm role}
    |]

renderForm :: Role -> Html
renderForm role = formFor role [hsx|
    {(textField #rolename)}
    {submitButton}
|]
