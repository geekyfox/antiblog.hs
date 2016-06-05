-- | Entrypoint module of `antisync` utility.
module Main(main) where

import Data.String
import System.Console.CmdArgs.Explicit
import Antiblog.Api
import Antiblog.Config
import Antiblog.ConfigMgt
import Antiblog.Syncer

type Task = Maybe SystemName -> Verbosity -> [String] -> IO ()

-- | What should be done according to command line arguments.
data Action = Action {
    invoke :: Task
    ,endpoint :: Maybe SystemName
    ,verbosity :: Verbosity
    ,files :: [String]
    }

verboseFlag :: Flag Action
verboseFlag = flagNone ["verbose"]
        (\a -> a { verbosity = Verbose })
        "Print skipped entries (except not modified ones)"

veryVerboseFlag :: Flag Action
veryVerboseFlag = flagNone ["very-verbose"]
        (\a -> a { verbosity = VeryVerbose })
        "Print all skipped entries"

targetArg :: Arg Action
targetArg = Arg
    {argValue = \v a -> Right $ a { endpoint = Just $ fromString v }
    ,argType = "<name of remote endpoint>"
    ,argRequire = True
    }

listFilesArg :: Arg Action
listFilesArg = Arg
    {argValue = \v a -> Right $ a { files = v:files a}
    ,argType = "<files to upload>"
    ,argRequire = True
    }

mkAction :: Task -> Action
mkAction task = Action task Nothing Normal []

data Options = Options {
    hasFiles :: Bool
    ,hasVerbosity :: Bool
    ,hasTarget :: Bool
    }
    
emptyOpts :: Options
emptyOpts = Options False False False

fixedArgs :: Options -> [Arg Action]
fixedArgs opts
    | hasTarget opts = [targetArg]
    | otherwise = []

extraArgs :: Options -> Maybe (Arg Action)
extraArgs opts
    | hasFiles opts = Just listFilesArg
    | otherwise = Nothing

flags :: Options -> [Flag Action]
flags opts
    | hasVerbosity opts = [verboseFlag, veryVerboseFlag]
    | otherwise = []

mkMode :: Name -> Task -> Help -> Options -> Mode Action
mkMode name task help opts = (modeEmpty $ mkAction task)
        {modeNames = [name]
        ,modeHelp = help
        ,modeArgs = (fixedArgs opts, extraArgs opts)
        ,modeGroupFlags = toGroup (flags opts)
        }

connect :: Maybe SystemName -> IO Client
connect Nothing = error "System name is missing"
connect (Just sys) = mkClient sys

statusMode :: Mode Action
statusMode = mkMode "status" go help opts
    where
        go sys v _ = connect sys >>= status "." >>= showStatus v
        help = "Compare status of local files to remote server"
        opts = emptyOpts { hasVerbosity = True, hasTarget = True }

syncMode :: Mode Action
syncMode = mkMode "sync" go help opts
    where
        go sys v fs = connect sys >>= sync v fs
        help = "Upload files to remote server"
        opts = Options { hasFiles = True, hasVerbosity = True, hasTarget = True }

pumpMode :: Mode Action
pumpMode = mkMode "pump" go help opts
    where
        go sys _ fs = connect sys >>= pump fs
        help = "Continuously pump files to server"
        opts = emptyOpts { hasFiles = True, hasTarget = True }

promoteMode :: Mode Action
promoteMode = mkMode "promote" go help opts
    where
        go sys _ fs = connect sys >>= promote fs
        help = "Schedule entries to appear on front page"
        opts = emptyOpts { hasFiles = True, hasTarget = True }

addRemoteMode :: Mode Action
addRemoteMode = mkMode "add-remote" go help opts
    where
        go _ _ _ = addRemoteConfig
        help = "Configure remote endpoint"
        opts = emptyOpts

helpAction :: Action
helpAction = mkAction (\_ _ _ -> print compositeMode)
    
compositeMode :: Mode Action
compositeMode = modes
    "antisync"
    helpAction
    "Command-line utility to synchronize antiblog posts"
    [statusMode, syncMode, pumpMode, promoteMode, addRemoteMode]

decideAction :: IO Action
decideAction = processArgs compositeMode

main :: IO ()
main = do
    a <- decideAction
    invoke a (endpoint a) (verbosity a) (files a)
