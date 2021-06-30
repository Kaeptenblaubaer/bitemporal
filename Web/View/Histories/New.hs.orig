module Web.View.Histories.New where
import Web.View.Prelude

data NewView = NewView { history :: History }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={HistoriesAction}>Histories</a></li>
                <li class="breadcrumb-item active">New History</li>
            </ol>
        </nav>
        <h1>New History</h1>
        {renderForm history}
    |]

renderForm :: History -> Html
renderForm history = formFor history [hsx|
    {(textField #latestversion)}
    {(textField #historyType)}
    {submitButton}
|]
