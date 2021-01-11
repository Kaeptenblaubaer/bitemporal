module Web.View.Userroles.New where
import Web.View.Prelude

data NewView = NewView { userrole :: Userrole }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={UserrolesAction}>Userroles</a></li>
                <li class="breadcrumb-item active">New Userrole</li>
            </ol>
        </nav>
        <h1>New Userrole</h1>
        {renderForm userrole}
    |]

renderForm :: Userrole -> Html
renderForm userrole = formFor userrole [hsx|
    {(textField #refuser)}
    {(textField #refrole)}
    {submitButton}
|]
