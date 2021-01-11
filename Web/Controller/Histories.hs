module Web.Controller.Histories where

import Web.Controller.Prelude
import Web.View.Histories.Index
import Web.View.Histories.New
import Web.View.Histories.Edit
import Web.View.Histories.Show

instance Controller HistoriesController where
    action HistoriesAction = do
        histories <- query @History |> fetch
        render IndexView { .. }

    action NewHistoryAction = do
        let history = newRecord
        render NewView { .. }

    action ShowHistoryAction { historyId } = do
        history :: history <- fetch historyId >>= fetchRelated #versions 
        render ShowView { .. }

    action EditHistoryAction { historyId } = do
        history <- fetch historyId
        render EditView { .. }

    action UpdateHistoryAction { historyId } = do
        history <- fetch historyId
        history
            |> buildHistory
            |> ifValid \case
                Left history -> render EditView { .. }
                Right history -> do
                    history <- history |> updateRecord
                    setSuccessMessage "History updated"
                    redirectTo EditHistoryAction { .. }

    action CreateHistoryAction = do
        let history = newRecord @History
        history
            |> buildHistory
            |> ifValid \case
                Left history -> render NewView { .. } 
                Right history -> do
                    history <- history |> createRecord
                    setSuccessMessage "History created"
                    redirectTo HistoriesAction

    action DeleteHistoryAction { historyId } = do
        history <- fetch historyId
        deleteRecord history
        setSuccessMessage "History deleted"
        redirectTo HistoriesAction

buildHistory history = history
    |> fill @["latestversion","historyType"]
