module Application.Helper.WorkflowProgress
where

{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-} 
{-# LANGUAGE OverloadedStrings #-} 
import IHP.ControllerPrelude
import GHC.Generics
import Data.Text.Encoding ( encodeUtf8 )
import Data.Text.Read as T (decimal)
import Data.Either ( fromRight )
import Data.Maybe ( fromJust )

data StateKeys = StateKeys  { history :: Maybe UUID , version :: Maybe Integer, state :: Maybe Integer } deriving (Show, Generic)
data WorkflowProgress = WorkflowProgress {contract :: Maybe StateKeys, partner:: Maybe StateKeys } deriving (Show, Generic)
instance FromJSON StateKeys
instance ToJSON StateKeys
instance FromJSON WorkflowProgress
instance ToJSON WorkflowProgress

viaJSONToText :: WorkflowProgress -> Text
viaJSONToText wfp = fromJust $ (decode.encode) wfp  