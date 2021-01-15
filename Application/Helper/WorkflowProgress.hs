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

data StateKeys = StateKeys  { history :: Maybe UUID , version :: Maybe Integer, state :: Maybe Integer } deriving (Show, Generic)
data WorkflowProgress = WorkflowProgress {contract :: Maybe StateKeys, partner:: Maybe StateKeys } deriving (Show, Generic)
instance FromJSON StateKeys
instance ToJSON StateKeys
instance FromJSON WorkflowProgress
instance ToJSON WorkflowProgress

wfpT :: Text
wfpT = "{\"partner\":null,\"contract\":{\"state\":4,\"history\":\"8b5fc6bc-3146-4d57-8587-8f12cdf2cca7\",\"version\":4}}"

wfp :: WorkflowProgress
wfp = WorkflowProgress (Just (StateKeys (Just "8b5fc6bc-3146-4d57-8587-8f12cdf2cca7") (Just 4) (Just 4))) Nothing


wfpJ :: Data.ByteString.Lazy.Internal.ByteString
wfpJ = encode wfp

wfpMB :: Maybe WorkflowProgress
wfpMB = decode wfpJ

wfp2 :: WorkflowProgress
wfp2 = fromJust wfpMB

wfpDB :: Value = fromJust $ decode $ encode wfp

