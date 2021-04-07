module Application.Script.List2Versions where
import Application.Script.Prelude
import GHC.Types
import Data.Tree
import Data.Maybe
list :: [(Integer , Integer)]
list = [(6,0),(5,0),(4,5),(3,4),(2,4),(1,99)]
list2 =  fromJust $ tail list
list3 = fromJust $ tail list2

root = (5,0)

isChild (node,_) (_,parent) = parent == node

mkTree :: (Integer,Integer) -> [(Integer,Integer)] -> (Tree (Integer,Integer),[(Integer ,Integer )])
mkTree root [] = (Node root [],[]) 
mkTree root rest = let  (children,rest2) = partition (isChild root) rest
                        (subforest,rest3) = mkForest2 children [] rest2 
    in (Node root subforest,rest3)

mkForest2 :: [(Integer, Integer)] -> [Tree (Integer, Integer)] -> [(Integer, Integer)] -> ( [Tree (Integer, Integer)], [(Integer, Integer)])
mkForest2 [] accu rest = (accu, rest)
mkForest2 (fstChild:siblings) accu rest  = 
        let (tree,newrest) = mkTree fstChild rest 
        in mkForest2 siblings (accu++[tree]) newrest

mkForest ::  [(Integer, Integer)] -> [Tree (Integer, Integer)] -> ( [Tree (Integer, Integer)], [(Integer, Integer)])
mkForest  [] akku               = (akku,[])
mkForest  (head:tail) akku      =
        let (tree,rest) = mkTree head tail 
        in mkForest rest (akku ++ [tree]) 



mk = mkForest2 [root] [] list3

mk2 = mkForest2 [(6,0)] [] list2


