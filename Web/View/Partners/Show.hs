module Web.View.Partners.Show where
import Web.View.Prelude

data ShowView = ShowView { partner :: Partner }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PartnersAction}>Partners</a></li>
                <li class="breadcrumb-item active">Show Partner</li>
            </ol>
        </nav>
        <h1>Show Partner</h1>
        <p>{partner}</p>
    |]
