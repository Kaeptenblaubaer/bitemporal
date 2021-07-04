#!/usr/bin/env run-script
module Application.Script.HowToMakeTree where
import Application.Script.Prelude
import Data.Tree

run :: Script
run = do
    let histoId :: Id History = Id "48c6b8df-31c9-4de6-8c1a-1353ddce1a78"
    versions :: [Version]<- sqlQuery "SELECT * FROM versions WHERE ref_history = ? order by createdat desc" (Only histoId)
    let (roots,descendants) :: ([Version],[Version]) = partition (\v -> isNothing $ get #refShadowedby v) versions
    forEach versions (\v -> putStrLn $ ">>>>> version: " ++ show v) 
    forEach roots (\v -> putStrLn $ ">>>>> valid: " ++ show v) 
    forEach descendants (\v -> putStrLn $ ">>>>> invalid: " ++ show v) 
    let forest = mkForest versions []
    putStrLn $ ">>>>>>>>>>TREE\n " ++ show forest


compId2refId :: Version -> Version -> Bool
compId2refId v ref = (Just $ get #id v) == get #refShadowedby ref


isChild node parent = compId2refId node parent  

mkTree :: Version -> [Version] -> (Tree Version,[Version])
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

