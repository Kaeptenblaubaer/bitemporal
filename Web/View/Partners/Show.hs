module Web.View.Partners.Show where
import Web.View.Prelude

data ShowView = ShowView { partner :: PartnerState }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PartnersAction}>Partners</a></li>
                <li class="breadcrumb-item active">Show PartnerState</li>
            </ol>
        </nav>
        <h1>Show PartnerState</h1>
        <p>{partner}</p>
    |]
