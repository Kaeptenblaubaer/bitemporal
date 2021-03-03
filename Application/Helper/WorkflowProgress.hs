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
import Generated.Types (Version, Workflow, Workflow'(progress) )

data StateKeys = StateKeys  { history :: Maybe UUID , version :: Maybe Integer, state :: Maybe Integer, shadowed :: Maybe (Integer,[Integer])} deriving (Show, Generic)
data WorkflowProgress = WorkflowProgress {contract :: Maybe StateKeys, partner:: Maybe StateKeys } deriving (Show, Generic)
instance FromJSON StateKeys
instance ToJSON StateKeys
instance FromJSON WorkflowProgress
instance ToJSON WorkflowProgress

getWfp :: Workflow -> Maybe WorkflowProgress
getWfp workflow  =  decode $ encode $ get #progress workflow

wfp :: WorkflowProgress
wfp = WorkflowProgress (Just (StateKeys (Just "8b5fc6bc-3146-4d57-8587-8f12cdf2cca7") (Just 4) (Just 4) Nothing)) Nothing

getContracthistoryId :: WorkflowProgress -> UUID
getContracthistoryId (WorkflowProgress (Just (StateKeys h v c _)) _) = fromJust h
getContractVersionIdMB :: WorkflowProgress -> Maybe Integer
getContractVersionIdMB (WorkflowProgress (Just (StateKeys h v c _)) _) = v

setContractVersionId :: Integer -> WorkflowProgress -> WorkflowProgress
setContractVersionId vid (WorkflowProgress (Just (StateKeys h v c sh)) partner) = WorkflowProgress (Just (StateKeys h (Just vid) c sh)) partner

setContractId :: Integer -> WorkflowProgress -> WorkflowProgress
setContractId sid (WorkflowProgress (Just (StateKeys h v c sh)) partner)  = WorkflowProgress (Just (StateKeys h v (Just sid) sh)) partner

getShadowed :: WorkflowProgress -> Maybe (Integer,[Integer])
getShadowed (WorkflowProgress (Just (StateKeys h v c shadowed)) partner) = shadowed

setShadowed :: WorkflowProgress -> (Integer,[Integer]) -> WorkflowProgress
setShadowed (WorkflowProgress (Just (StateKeys h v c sh)) partner) shadowed = WorkflowProgress (Just (StateKeys h v c (Just shadowed))) partner

wfpJ :: Data.ByteString.Lazy.Internal.ByteString
wfpJ = encode wfp

wfpMB :: Maybe WorkflowProgress
wfpMB = decode wfpJ

wfp2 :: WorkflowProgress
wfp2 = fromJust wfpMB

wfpDB :: Value = fromJust $ decode $ encode wfp

setWfp :: Workflow -> WorkflowProgress -> Workflow
setWfp wf wfp = wf |> set #progress ( fromJust $ decode $ encode wfp )

wf2 = setWfp (newRecord ::Workflow) (setShadowed wfp (0,[0]))