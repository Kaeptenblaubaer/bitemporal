module Web.View.Versions.New where
import Web.View.Prelude

data NewView = NewView { version :: Version }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={VersionsAction}>Versions</a></li>
                <li class="breadcrumb-item active">New Version</li>
            </ol>
        </nav>
        <h1>New Version</h1>
        {renderForm version}
    |]

renderForm :: Version -> Html
renderForm version = formFor version [hsx|
    {(textField #refHistory)}
    {(textField #validfrom)}
    {(textField #committed)}
    {(textField #refShadowedby)}
    {submitButton}
|]
