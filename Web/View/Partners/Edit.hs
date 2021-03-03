module Web.View.Partners.Edit where
import Web.View.Prelude

data EditView = EditView { partner :: Partner }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PartnersAction}>Partners</a></li>
                <li class="breadcrumb-item active">Edit Partner</li>
            </ol>
        </nav>
        <h1>Edit Partner</h1>
        {renderForm partner}
    |]

renderForm :: Partner -> Html
renderForm partner = formFor partner [hsx|
    {(textField #refvalidfromversion)}
    {(textField #refvalidthruversion)}
    {(textField #refhistory)}
    {(textField #content)}
    {submitButton}
|]
