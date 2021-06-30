module Web.View.Tariffs.Show where
import Web.View.Prelude

data ShowView = ShowView { tariff :: TariffState }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TariffsAction}>Tariffs</a></li>
                <li class="breadcrumb-item active">Show TariffState</li>
            </ol>
        </nav>
        <h1>Show TariffState</h1>
        <p>{tariff}</p>
    |]
