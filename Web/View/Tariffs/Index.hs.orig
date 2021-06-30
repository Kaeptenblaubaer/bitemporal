module Web.View.Tariffs.Index where
import Web.View.Prelude

data IndexView = IndexView { tariffs :: [Tariff] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={TariffsAction}>Tariffs</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewTariffAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Tariff</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach tariffs renderTariff}</tbody>
            </table>
        </div>
    |]


renderTariff tariff = [hsx|
    <tr>
        <td>{tariff}</td>
        <td><a href={ShowTariffAction (get #id tariff)}>Show</a></td>
        <td><a href={EditTariffAction (get #id tariff)} class="text-muted">Edit</a></td>
        <td><a href={DeleteTariffAction (get #id tariff)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
