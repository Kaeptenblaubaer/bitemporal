module Web.View.Tariffs.Show where
import Web.View.Prelude

data ShowView = ShowView { tariff :: Tariff }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TariffsAction}>Tariffs</a></li>
                <li class="breadcrumb-item active">Show Tariff</li>
            </ol>
        </nav>
        <h1>Show Tariff</h1>
        <p>{tariff}</p>
    |]
