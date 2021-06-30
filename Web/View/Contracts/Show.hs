module Web.View.Contracts.Show where
import Web.View.Prelude

data ShowView = ShowView { contract :: ContractState }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">Show ContractState</li>
            </ol>
        </nav>
        <h1>Show ContractState</h1>
        <table><thead><tr><th>history</th><th>validfromversion</th><th>validthruversion</th></tr><tr><th>content</th></tr></thead><tbody>
        <tr><td>{get #refEntity contract}</td><td>{get #refValidfromversion contract}</td><td>{get #refValidthruversion contract}</td></tr>
        <tr><td>{get #content contract}</td></tr>
        </tbody></table>
    |]

   