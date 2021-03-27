#!/usr/bin/env run-script
module Application.Script.HowToLock where
import Application.Script.Prelude

run :: Script
run = do
    let histoId :: Id History = Id "5d76a9c0-d682-4287-9af7-f9a670261a18"
    histos :: [History]<- sqlQuery "SELECT * FROM histories WHERE id = ? AND locked = false FOR UPDATE SKIP LOCKED" (Only histoId)
    case head histos of 
        Nothing -> putStrLn "entity is locked or doesn't exist"
        Just h -> putStrLn (show h)