module Application.Helper.Controller -- (createHistory, getKey, queryMutableState, StateKeys, today, UUID, viaJSONToText, WorkflowProgress
    -- To use the built in login:
    -- module IHP.LoginSupport.Helper.Controller
--) 
where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-} 
{-# LANGUAGE OverloadedStrings #-}
-- Here you can add functions which are available in all your controllers
import IHP.ControllerPrelude
-- To use the built in login:
-- import IHP.LoginSupport.Helper.Controller
import Generated.Types
import Application.Helper.WorkflowProgress
import Database.PostgreSQL.Simple ( Query, ToRow )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Text.Read as T (decimal)
import Data.Maybe ( fromJust )
import GHC.Generics
import qualified IHP.Log as Log
import IHP.Log.Types
import Application.Helper.VersionTree

today :: IO (Day) -- :: (year,month,day)
today = getCurrentTime >>= return . utctDay


class (KnownSymbol (GetTableName rec), rec ~ GetModelByTableName (GetTableName rec), PrimaryKey (GetTableName rec) ~ Integer, Record rec, CanCreate rec,Fetchable (QueryBuilder (GetTableName rec))  rec, FromRow rec,
    HasField "id" rec (Id rec), Show (PrimaryKey (GetTableName rec)), HasField "refHistory" rec (Id History),SetField "refHistory" rec (Id History),
    HasField "refValidfromversion" rec (Id Version), SetField "refValidfromversion" rec (Id Version),
    HasField "refValidthruversion" rec (Maybe(Id Version)), SetField "refValidthruversion" rec (Maybe (Id Version)),

    HasField "content" rec Text, SetField "content" rec Text) => CanVersion rec 
    where

    getKey :: rec -> Integer
    default getKey :: rec -> Integer
    getKey m = case decimal $ recordToInputValue m of
                                    Left _ -> -1
                                    Right ( i , _) -> i
    queryMutableState :: (?modelContext::ModelContext, ?context::context, LoggingProvider context )=> Workflow -> IO (rec,[Version])
    queryMutableState workflow =  do
        
        mutable :: (Version,[Version]) <- queryVersionMutableValidfrom workflow 
        let h = get #refHistory $ fst mutable
        Log.info $ "querymutable " ++ show h
        let v = get #id $ fst mutable
        mstate <- query @rec |> filterWhere(#refHistory, h) |> filterWhereSql (#refValidfromversion, encodeUtf8("<= " ++ (show v))) |>
                            queryOr 
                                 (filterWhereSql (#refValidthruversion, encodeUtf8("> " ++ (show v))))
                                 (filterWhereSql (#refValidthruversion, "is null")) |> fetchOne
        
        pure (mstate,snd mutable)

    createHistory :: (?modelContext::ModelContext, ?context::context, LoggingProvider context ) => Workflow -> rec -> IO rec
    createHistory workflow state = do
        Log.info $ "createHistory for workflow: " ++ (show (get #id workflow))
        history ::History <- newRecord |> set #historyType (get #historyType workflow) |> set #refOwnedByWorkflow (Just $ get #id workflow)|> createRecord
        let historyUUID ::UUID = bubu $ get #id history
                                    where bubu (Id uuid) = uuid
        version :: Version <- newRecord |> set #refHistory (get #id history) |>  set #validfrom (get #validfrom workflow) |> createRecord
        let versionId :: Integer = bubu $ get #id version
                                    where bubu (Id intid) = intid
        state ::rec <- state |> set #refHistory (get #id history) |> set #refValidfromversion (get #id version) |> createRecord
        uptodate ::Workflow <- workflow |> set #progress ( toJSON $ WorkflowProgress  (Just $ StateKeys (Just historyUUID)  (Just versionId)  (Just (getKey state)) Nothing )  Nothing ) |> updateRecord
        putStrLn ("hier ist Workflow mit JSON " ++ (show (get #progress uptodate)))
        pure state
 
    mutateHistory :: (?modelContext::ModelContext) => Workflow -> rec -> IO rec
    mutateHistory workflow state = do
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
        let versionIdMB = getContractVersionIdMB wfprogress
        case versionIdMB of
            Just v -> pure state
            Nothing -> do
                putStrLn "mutateHistory Update new Version"
                let validfrom = tshow $ get #validfrom workflow
                let historyId =  get #refHistory state
                version :: Version <- newRecord |> set #refHistory historyId |> set #validfrom (get #validfrom workflow) |> createRecord
                newState :: rec <- newRecord |> set #refHistory historyId |> set #refValidfromversion (get #id version) |>
                    set #content (get #content state) |> createRecord
                workflow <- setWfp workflow ( upd (fromId $ get #id version ) (fromId $ get #id newState ) wfprogress) |> updateRecord                  
                pure newState
                    where upd vid sid workflow = ((setContractId sid).(setContractVersionId vid)) workflow

instance CanVersion Contract

queryVersionMutableValidfrom :: (?modelContext::ModelContext) => Workflow -> IO (Version,[Version])
queryVersionMutableValidfrom workflow = do
        putStrLn ( "queryVersionMutableValidfrom Workflow=" ++ (show workflow) )
        let wfprogress :: WorkflowProgress = fromJust $ getWfp workflow
        let validfrom = tshow $ get #validfrom workflow
        let historyId =  getContracthistoryId wfprogress
        let q :: Query = "SELECT * FROM versions v WHERE v.id in (SELECT max(id) FROM versions where ref_history = ? and validfrom <= ?)"
        let p :: (Id History, Text) = (Id historyId, validfrom)
        vs :: [Version]  <- sqlQuery  q p
        let versionId = get #id $ fromJust $ head vs
        putStrLn ( "queryVersionMutableValidfrom versionid=" ++ (show versionId ))
        let q2 :: Query = "SELECT * FROM versions v WHERE ref_history = ? and v.id > ? and validfrom > ?"
        let p2 :: (Id History, Id Version,Text) = (Id historyId, versionId, validfrom)
        shadowed :: [Version]  <- sqlQuery  q2 p2
        let shadowedIds :: [Integer] = map (getKey .(get #id)) shadowed  
        putStrLn ( "queryVersionMutableValidfrom shadowed=" ++ (show shadowed ))
        workflow :: Workflow <- setWfp workflow (setShadowed wfprogress (getKey versionId, shadowedIds)) |> updateRecord
        pure $ (fromJust $ head vs, shadowed)
            where getKey (Id key) = key

getCurrentWorkflow :: (?context::ControllerContext, ?modelContext::ModelContext) => IO Workflow
getCurrentWorkflow  = do
    id <- getSessionUUID "workflowId"
    putStrLn ("current workflowid = " ++ (show id))
    wf :: Workflow <- fetch (Id (fromJust id))
    pure wf

getCurrentWorkflowId :: (?context::ControllerContext, ?modelContext::ModelContext) => IO (Id Workflow)
getCurrentWorkflowId = do
        workflow :: Workflow <- getCurrentWorkflow
        pure (get #id workflow)
    
setCurrentWorkflowId :: (?context::ControllerContext) => Workflow -> IO ()
setCurrentWorkflowId workflow = do
    oldid <- getSessionUUID "workflowId"
    putStrLn ("old workflowid = " ++ (show oldid))
    setSession "workflowId" (show (get #id workflow)) 
    newid <- getSessionUUID "workflowId"
    putStrLn ("current workflowid = " ++ (show newid))

fromId :: Id' table -> PrimaryKey table
fromId (Id key) = key