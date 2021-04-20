module Web.View.Workflows.New where
import Web.View.Prelude
import Web.Enums

data NewView = NewView { workflow :: Workflow }



instance View NewView where
    html NewView { .. } = renderModal Modal
                { modalTitle = "New Workflow"
                , modalCloseUrl = pathTo WorkflowsAction
                , modalFooter = Nothing
                , modalContent = case  getWfType workflow   of
                            WftypeNew ->  renderFormNew workflow
                            WftypeUpdate -> renderFormUpd workflow
                }

renderFormNew :: Workflow -> Html
renderFormNew workflow =  formFor workflow [hsx|
    {(textField #refUser)}
    {textField #progress}
    {dateField #validfrom}
    {selectField #historyType [HistorytypeContract,HistorytypePartner,HistorytypeTariff]}
    {submitButton}
|]

renderFormUpd :: Workflow -> Html
renderFormUpd workflow = formFor workflow [hsx|
    {(textField #refUser)}
    {textField #progress}
    {dateField #validfrom}
    {textField #historyType}
    {textField #workflowType}
    {submitButton}
|]


getWfType :: HasField "workflowType" model value => model -> value
getWfType workflow = get #workflowType workflow
