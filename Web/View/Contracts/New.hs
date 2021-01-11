module Web.View.Contracts.New where
import Web.View.Prelude
data NewView = NewView { contract :: Contract , workflow :: Workflow }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ContractsAction}>Contracts</a></li>
                <li class="breadcrumb-item active">New Contract</li>
            </ol>
        </nav>
        <h1>New Contract</h1>
        {renderForm contract workflow }
 
    |]

renderForm :: Contract -> Workflow ->Html
renderForm contract workflow = formFor contract [hsx|
    {(textField #validfromversion)}
    {(textField #validthruversion)}
    {(textField #refhistory)}
    {(textField #content ) }
    {submitButton}
|] 
