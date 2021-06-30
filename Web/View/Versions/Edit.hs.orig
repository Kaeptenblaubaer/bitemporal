module Web.View.Versions.Edit where
import Web.View.Prelude

data EditView = EditView { version :: Version }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={VersionsAction}>Versions</a></li>
                <li class="breadcrumb-item active">Edit Version</li>
            </ol>
        </nav>
        <h1>Edit Version</h1>
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
