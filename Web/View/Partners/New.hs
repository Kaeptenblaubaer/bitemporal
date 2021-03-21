module Web.View.Partners.New where
import Web.View.Prelude

data NewView = NewView { partner :: Partner }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PartnersAction}>Partners</a></li>
                <li class="breadcrumb-item active">New Partner</li>
            </ol>
        </nav>
        <h1>New Partner</h1>
        {renderForm partner}
    |]

renderForm :: Partner -> Html
renderForm partner = formFor partner [hsx|
    {(textField #refValidfromversion)}
    {(textField #refValidthruversion)}
    {(textField #refHistory)}
    {(textField #content)}
    {submitButton}
|]
