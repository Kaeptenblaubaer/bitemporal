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

today :: IO (Day) -- :: (year,month,day)
today = getCurrentTime >>= return . utctDay


class (KnownSymbol (GetTableName rec), rec ~ GetModelByTableName (GetTableName rec), Record rec, CanCreate rec,Fetchable (QueryBuilder (GetTableName rec))  rec, FromRow rec, HasField "id" rec (Id rec), Show (PrimaryKey (GetTableName rec)), HasField "refhistory" rec (Id History),SetField "refhistory" rec (Id History), HasField "validfromversion" rec (Id Version), SetField "validfromversion" rec (Id Version), HasField "validthruversion" rec (Maybe(Id Version))) => CanVersion rec where
    getKey :: rec -> Integer
    default getKey :: rec -> Integer
    getKey m = case decimal $ recordToInputValue m of
                                    Left _ -> -1
                                    Right ( i , _) -> i

    queryMutableState :: (?modelContext::ModelContext)=> Workflow -> IO (rec,[Version])
    queryMutableState workflow =  do
        mutable :: (Version,[Version]) <- queryVersionMutableValidfrom workflow 
        let h = get #refhistory $ fst mutable
        let v = get #id $ fst mutable
        mstate <- query @rec |> filterWhere(#refhistory, h) |> filterWhereSql (#validfromversion, encodeUtf8("<= " ++ (show v))) |>
                            queryOr 
                                 (filterWhereSql (#validthruversion, encodeUtf8("> " ++ (show v))))
                                 (filterWhereSql (#validthruversion, "is null")) |> fetchOne
        
        pure (mstate,snd mutable)

    createHistory :: (?modelContext::ModelContext) => Workflow -> IO rec
    createHistory workflow = do
                        history ::History <- newRecord |> set #historyType (get #historyType workflow) |> createRecord
                        let historyUUID ::UUID = bubu $ get #id history
                                                    where bubu (Id uuid) = uuid
                        version :: Version <- newRecord |> set #refhistory (get #id history) |>  set #validfrom (get #validfrom workflow) |> createRecord
                        let versionId :: Integer = bubu $ get #id version
                                                    where bubu (Id intid) = intid
                        state ::rec <- newRecord |> set #refhistory (get #id history) |> set #validfromversion (get #id version) |> createRecord
                        uptodate ::Workflow <- workflow |> set #progress (show . encode $ WorkflowProgress (Just(StateKeys (Just historyUUID) (Just versionId) (Just (getKey state)) )) Nothing) |> updateRecord
                        putStrLn ("hier ist Workflow mit JSON " ++ (show (get #progress uptodate)))
                        pure state
 

instance CanVersion Contract

queryVersionMutableValidfrom :: (?modelContext::ModelContext) => Workflow -> IO (Version,[Version])
queryVersionMutableValidfrom workflow = do
        let wfprogress :: WorkflowProgress = fromJust $ decode.encode $ get #progress workflow
        let validfrom = tshow $ get #validfrom workflow
        let historyId =  fromJust $ getContractKeys wfprogress
                where getContractKeys (WorkflowProgress (Just (StateKeys h _ _ )) _) = h
        let q :: Query = "SELECT * FROM versions v WHERE v.id in (SELECT max(id) FROM versions where refhistory = ? and validfrom <= ?)"
        let p :: (Id History,Text) = (Id historyId,validfrom)
        vs :: [Version]  <- sqlQuery  q p
        let q2 :: Query = "SELECT v FROM versions v WHERE refhistory = ? and v.id > ? and validfrom > ?"
        shadowed :: [Version]  <- sqlQuery  q2 p
        pure $ (fromJust $ head vs, shadowed)

 

