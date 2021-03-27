#!/usr/bin/env run-script
module Application.Script.HowToMakeTree where
import Application.Script.Prelude
import Data.Tree

run :: Script
run = do
    let histoId :: Id History = Id "48c6b8df-31c9-4de6-8c1a-1353ddce1a78"
    versions :: [Version]<- sqlQuery "SELECT * FROM versions WHERE ref_history = ? order by createdat" (Only histoId)
    let (roots,descendants) :: ([Version],[Version]) = partition (\v -> isNothing $ get #refShadowedby v) versions
    forEach versions (\v -> putStrLn $ ">>>>> version: " ++ show v) 
    forEach roots (\v -> putStrLn $ ">>>>> valid: " ++ show v) 
    forEach descendants (\v -> putStrLn $ ">>>>> invalid: " ++ show v) 
    let forest = build versions 
    putStrLn $ ">>>>>>>>>>TREE\n " ++ show forest


compId2refId :: Version -> Version -> Bool
compId2refId v ref = (Just $ get #id v) == get #refShadowedby ref


build ::  [Version] -> Forest (Id Version, Maybe (Id Version))
build [] = [] 
build (v : vs) = let (theChildren,notTheChildren) = partition (\ref -> (Just $ get #id v) == get #refShadowedby ref) vs
    in (mknode v theChildren : build notTheChildren)
    
mknode v children = Node (get #id v, get #refShadowedby v) (build children)

