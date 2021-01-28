module Web.View.Contracts.Show where
import Web.View.Prelude

data ShowView = ShowView { contract :: Contract }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">Show Contract</li>
            </ol>
        </nav>
        <h1>Show Contract</h1>
        <p>{contract}</p>
    |]
