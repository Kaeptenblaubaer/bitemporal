module Web.View.Roles.New where
import Web.View.Prelude

data NewView = NewView { role :: Role }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={RolesAction}>Roles</a></li>
                <li class="breadcrumb-item active">New Role</li>
            </ol>
        </nav>
        <h1>New Role</h1>
        {renderForm role}
    |]

renderForm :: Role -> Html
renderForm role = formFor role [hsx|
    {(textField #rolename)}
    {submitButton}
|]
