
-- | Entrypoint module of `antisync` utility.
module Main(main) where

import Antiblog.Config
import Antiblog.Syncer
import Antisync.CmdLine

main :: IO ()
main = do
    a <- decideAction
    ep <- loadOrDie (actionEndpoint a)
    let verbose = actionVerbosity a
    let files = actionFiles a
    case actionType a of
        DoNothing ->
            putStrLn helpMessage
        Status ->
            status "." ep >>= showStatus verbose
        Sync ->
            sync verbose files ep
        Promote ->
            promote files ep
        Pump ->
            pump files ep
