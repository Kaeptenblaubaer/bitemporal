module Web.View.Partners.Index where
import Web.View.Prelude

data IndexView = IndexView { partners :: [Partner] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PartnersAction}>Partners</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPartnerAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Partner</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach partners renderPartner}</tbody>
            </table>
        </div>
    |]


renderPartner partner = [hsx|
    <tr>
        <td>{partner}</td>
        <td><a href={ShowPartnerAction (get #id partner)}>Show</a></td>
        <td><a href={EditPartnerAction (get #id partner)} class="text-muted">Edit</a></td>
        <td><a href={DeletePartnerAction (get #id partner)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
