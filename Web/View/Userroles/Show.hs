module Web.View.Userroles.Show where
import Web.View.Prelude

data ShowView = ShowView { userrole :: Userrole }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={UserrolesAction}>Userroles</a></li>
                <li class="breadcrumb-item active">Show Userrole</li>
            </ol>
        </nav>
        <h1>Show Userrole</h1>
        <p>{userrole}</p>
    |]
