module Web.View.Histories.Show where
import Web.View.Prelude
import Data.Tree
data ShowView = ShowView { history :: History , versions :: [Version] }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={HistoriesAction}>Histories</a></li>
                <li class="breadcrumb-item active">Show History</li>
            </ol>
        </nav>
        <h1>Show History</h1>
        <h2 > Tree representation of bitemporal versioning histories</h2>
        <p>reference time is displayed vertically running down from old to new. Indentation represents
        invalidation of timelines by retrospective mutatations </p>
        <p>Click on the arrow(s) to open or close the tree branches, that is: show or hide invalidated timelines</p>
        <ol id="history" >
          { forEach (fst $ mkForest versions []) renderTree } 
        </ol>
    |]

renderTree :: Tree Version -> Html
renderTree n  = case subForest n of
       [] ->  [hsx| 
                    <li>
                            {renderLabel n}
                    </li>
                |]
       _  ->  [hsx|  
                    <li>{renderLabel n} 
                        <span class="caret" ></span>
                        <ol class="nested">
                          { forEach (subForest n) renderTree } 
                        </ol>
                    </li>
                |]


renderLabel :: Tree Version -> Html
renderLabel n = render (rootLabel n)
    where render version = [hsx| <div><table><tr>
            <td>{get #id version}</td><td>{get #validfrom version}</td><td><a href={pathTo(NewWorkflowAction) <> "?historyId=" ++ (show (get #refHistory version))}>Start Mutation Workflow</a></td>
            </tr></table></div>
          |] 

-- <a href={pathTo(NewWorkflowAction) <> "?historyId=" ++ (show (get #id refHistory))}>Start Mutation Workflow</a>


bubu history = do
    let versionForest = mkForest (get #versions history) []
    putStrLn $ ">>>>>>>>>>TREE\n " ++ show versionForest

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