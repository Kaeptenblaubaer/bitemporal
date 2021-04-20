module Application.Helper.WorkflowProgress
where

{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
import IHP.ControllerPrelude
import GHC.Generics
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text.Encoding ( encodeUtf8, decodeUtf8 )
import Data.Text.Read as T (decimal)
import Data.Either ( fromRight )
import Data.Maybe ( fromJust )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Text (replace)
import Generated.Types (Version, Workflow, Workflow'(progress), Contract , Partner, Tariff, HistoryType(..) )

data StateKeys = StateKeys  { history :: Maybe UUID , version :: Maybe Integer, state :: Maybe Integer, shadowed :: Maybe (Integer,[Integer])} deriving (Show, Generic)
data WorkflowProgress = WorkflowProgress {contract :: Maybe StateKeys, partner:: Maybe StateKeys , tariff :: Maybe StateKeys } deriving (Show, Generic)
instance FromJSON StateKeys
instance ToJSON StateKeys
instance FromJSON WorkflowProgress
instance ToJSON WorkflowProgress

getWfp :: Workflow -> Maybe WorkflowProgress
getWfp workflow  =  decode $ encode $ get #progress workflow

initialWfpV :: HistoryType -> UUID -> Value
initialWfpV histoType historyId = fromJust $ decode $ encode $ case histoType of
    HistorytypeContract -> WorkflowProgress { contract = Just $ StateKeys (Just historyId) Nothing Nothing Nothing , partner = Nothing, tariff = Nothing}
    HistorytypePartner -> WorkflowProgress {partner = Just $ StateKeys (Just historyId) Nothing Nothing Nothing , contract = Nothing, tariff = Nothing}
    HistorytypeTariff -> WorkflowProgress {tariff = Just $ StateKeys (Just historyId) Nothing Nothing Nothing, contract = Nothing , partner = Nothing }

updateWfpV :: Value -> HistoryType -> UUID -> Integer -> Integer -> Value
updateWfpV wfp histoType historyId versionId stateId =
    let stateKeys = Just (StateKeys (Just historyId)  (Just versionId) (Just stateId) Nothing)
        wfp = case histoType of
            HistorytypeContract -> wfp { contract = stateKeys }
            HistorytypePartner -> wfp {partner = stateKeys }
            HistorytypeTariff -> wfp {tariff = stateKeys }
    in fromJust $ decode $ encode wfp

setWfp :: Workflow -> WorkflowProgress -> Workflow
setWfp wf wfp = wf |> set #progress ( fromJust $ decode $ encode wfp )

getStatehistoryId :: WorkflowProgress -> HistoryType -> UUID
getStatehistoryId histoType wfp = 
    let xtractH (WorkflowProgress (Just (StateKeys h v c _)) _ _) = fromJust h
        stateKeys = case histoType of
            HistorytypeContract -> contract wfp
            HistorytypePartner -> partner wfp
            HistorytypeTariff -> tariff wfp
    in xtractH stateKeys
            
            


getContractVersionIdMB :: WorkflowProgress -> Maybe Integer
getContractVersionIdMB (WorkflowProgress (Just (StateKeys h v c _)) _ _) = v
getContractIdMB :: WorkflowProgress -> Maybe Integer
getContractIdMB (WorkflowProgress (Just (StateKeys h v c _)) _ _) = c


getContractId :: Workflow -> Id Contract
getContractId workflow =  Id $ fromJust $ getContractIdMB $ fromJust $ (decode . encode) $ get  #progress workflow

setContractVersionId :: Integer -> WorkflowProgress -> WorkflowProgress
setContractVersionId vid (WorkflowProgress (Just (StateKeys h v c sh)) partner tariff) = WorkflowProgress (Just (StateKeys h (Just vid) c sh)) partner tariff

setContractId :: Integer -> WorkflowProgress -> WorkflowProgress
setContractId sid (WorkflowProgress (Just (StateKeys h v c sh)) partner tariff)  = WorkflowProgress (Just (StateKeys h v (Just sid) sh)) partner tariff

getShadowed :: WorkflowProgress -> Maybe (Integer,[Integer])
getShadowed (WorkflowProgress (Just (StateKeys h v c shadowed)) partner tariff) = shadowed

setShadowed :: WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
setShadowed (WorkflowProgress (Just (StateKeys h v c sh)) partner tariff) shadowed = WorkflowProgress (Just (StateKeys h v c (Just shadowed))) partner tariff
