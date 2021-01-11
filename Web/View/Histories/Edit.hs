module Web.View.Histories.Edit where
import Web.View.Prelude

data EditView = EditView { history :: History }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={HistoriesAction}>Histories</a></li>
                <li class="breadcrumb-item active">Edit History</li>
            </ol>
        </nav>
        <h1>Edit History</h1>
        {renderForm history}
    |]

renderForm :: History -> Html
renderForm history = formFor history [hsx|
    {(textField #latestversion)}
    {(textField #historyType)}
    {submitButton}
|]
