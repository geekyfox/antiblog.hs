-- | Entrypoint module of `antisync` utility.
module Main(main) where

import Data.String
import System.Console.CmdArgs.Explicit
import Antiblog.Api
import Antiblog.Config
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

mkMode :: Name -> Task -> Help -> Bool -> Bool -> Mode Action
mkMode name task help hasFiles hasVerbosity = mode
        {modeNames = [name]
        ,modeHelp = help
        ,modeArgs = args
        ,modeGroupFlags = toGroup flags
        }
    where
        mode = modeEmpty (mkAction task)
        args
            | hasFiles = ([targetArg], Just listFilesArg)
            | otherwise = ([targetArg], Nothing)
        flags
            | hasVerbosity = [verboseFlag, veryVerboseFlag]
            | otherwise = []

statusMode :: Mode Action
statusMode = mkMode "status" go help False True
    where
        go sys v _ = mkClient sys >>= status "." >>= showStatus v
        help = "Compare status of local files to remote server"

syncMode :: Mode Action
syncMode = mkMode "sync" go help True True
    where
        go sys v fs = mkClient sys >>= sync v fs
        help = "Upload files to remote server"

pumpMode :: Mode Action
pumpMode = mkMode "pump" go help True False
    where
        go sys _ fs = mkClient sys >>= pump fs
        help = "Continuously pump files to server"

promoteMode :: Mode Action
promoteMode = mkMode "promote" go help True False
    where
        go sys _ fs = mkClient sys >>= promote fs
        help = "Schedule entries to appear on front page"

helpAction :: Action
helpAction = mkAction (\_ _ _ -> print compositeMode)
    
compositeMode :: Mode Action
compositeMode = modes
    "antisync"
    helpAction
    "Command-line utility to synchronize antiblog posts"
    [statusMode, syncMode, pumpMode, promoteMode]

decideAction :: IO Action
decideAction = processArgs compositeMode

main :: IO ()
main = do
    a <- decideAction
    invoke a (endpoint a) (verbosity a) (files a)
