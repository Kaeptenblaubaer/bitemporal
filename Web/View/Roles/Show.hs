module Web.View.Roles.Show where
import Web.View.Prelude

data ShowView = ShowView { role :: Role }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={RolesAction}>Roles</a></li>
                <li class="breadcrumb-item active">Show Role</li>
            </ol>
        </nav>
        <h1>Show Role</h1>
        <p>{role}</p>
    |]
