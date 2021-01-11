module Web.View.Userroles.Edit where
import Web.View.Prelude

data EditView = EditView { userrole :: Userrole }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={UserrolesAction}>Userroles</a></li>
                <li class="breadcrumb-item active">Edit Userrole</li>
            </ol>
        </nav>
        <h1>Edit Userrole</h1>
        {renderForm userrole}
    |]

renderForm :: Userrole -> Html
renderForm userrole = formFor userrole [hsx|
    {(textField #refuser)}
    {(textField #refrole)}
    {submitButton}
|]
