module Web.Controller.Histories where

import Web.Controller.Prelude
import Web.View.Histories.Index
import Web.View.Histories.New
import Web.View.Histories.Edit
import Web.View.Histories.Show
import Data.Tree

instance Controller HistoriesController where
    action HistoriesAction = do
        histories <- query @History |> fetch
        render IndexView { .. }

    action NewHistoryAction = do
        let history = newRecord
        render NewView { .. }

    action ShowHistoryAction { historyId } = do
        history :: history <- fetch historyId >>= fetchRelated #versions 
        versions :: [Version]<- sqlQuery "SELECT * FROM versions WHERE ref_history = ? order by createdat desc" (Only historyId)
        let (roots,descendants) :: ([Version],[Version]) = partition (\v -> isNothing $ get #refShadowedby v) versions
        forEach versions (\v -> putStrLn $ ">>>>> version: " ++ show v) 
        forEach roots (\v -> putStrLn $ ">>>>> valid: " ++ show v) 
        forEach descendants (\v -> putStrLn $ ">>>>> invalid: " ++ show v) 
        let forest = mkForest versions []
        putStrLn $ ">>>>>>>>>>TREE\n " ++ show forest
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

compId2refId :: Version -> Version -> Bool
compId2refId v ref = (Just $ get #id v) == get #refShadowedby ref

isChild node parent = compId2refId node parent  

mkTree root [] = (Node root [],[]) 
mkTree root rest = let  (children,rest2) = partition (isChild root) rest
                        (subforest,rest3) = mkSubForest children [] rest2 
    in (Node root subforest,rest3)

mkSubForest :: [Version] -> [Tree Version] -> [Version] -> ( [Tree Version], [Version])
mkSubForest [] accu rest = (accu, rest)
mkSubForest (fstChild:siblings) accu rest  = 
        let (tree,newrest) = mkTree fstChild rest 
        in mkSubForest siblings (accu++[tree]) newrest

mkForest ::  [Version] -> [Tree Version] -> ( [Tree Version], [Version])
mkForest  [] akku               = (akku,[])
mkForest  (head:tail) akku      =
        let (tree,rest) = mkTree head tail 
        in mkForest rest (akku ++ [tree]) 