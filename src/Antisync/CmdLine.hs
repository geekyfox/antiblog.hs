
-- | Entrypoint module of `antisync` utility.
module Antisync.CmdLine
    ( Verbosity(Normal,Verbose,VeryVerbose)
    , ActionType(
         Status
        ,Sync
        ,Pump
        ,Promote
        ,DoNothing
        )
    , Action(actionType,actionEndpoint,actionVerbosity,actionFiles)
    , decideAction
    , helpMessage
    , shouldIgnore
    )

where

import System.Console.CmdArgs.Explicit

import Utils.Data.Outcome
import Utils.Data.Tagged

import Antisync.Config(SystemName)

-- | Verbosity level.
data Verbosity = 
    -- | Don't print `Skip` messages.
      Normal
    -- | Don't print "entry is not modified messages".
    | Verbose      
    -- | Print all messages.
    | VeryVerbose

-- | Matches an entry against verbosity setting.
shouldIgnore :: Verbosity -> Outcome a -> Bool
shouldIgnore Verbose (Skip "Not modified") = True
shouldIgnore Normal  (Skip _)              = True
shouldIgnore _       _                     = False

data ActionType = Status | Sync | Pump | Promote | DoNothing

-- | What should be done according to command line arguments.
data Action = Action {
     actionType      :: ActionType
    ,actionEndpoint  :: Maybe SystemName
    ,actionVerbosity :: Verbosity
    ,actionFiles     :: [String]
}

mkAction :: ActionType -> Action
mkAction t = Action t Nothing Normal []

ignoreArg :: Arg a
ignoreArg = flagArg (\_ x -> Right x) ""

targetFlag :: Flag Action
targetFlag = flagReq ["target"]
                     mutate
                     "<name of remote endpoint>"
                     "Use production server"
    where
        mutate value a = Right $ a { actionEndpoint = Just $ wrap value }
    
verboseFlag :: Flag Action
verboseFlag = flagNone
              ["verbose"]
              mutate
              "Print skipped entries (except not modified ones)"
    where mutate a = a { actionVerbosity = Verbose }

veryVerboseFlag :: Flag Action
veryVerboseFlag = flagNone
                  ["very-verbose"]
                  mutate
                  "Print all skipped entries"
    where mutate a = a { actionVerbosity = VeryVerbose }

listFilesArg :: Arg Action
listFilesArg = 
    let upd v a = Right $ a { actionFiles = v:actionFiles a }
    in Arg {
        argValue   = upd,
        argType    = "<files to upload>",
        argRequire = True
    }

statusMode :: Mode Action
statusMode = mode
    "status"
    (mkAction Status)
    "Compare status of local files to remote server"
    ignoreArg
    [targetFlag, verboseFlag, veryVerboseFlag]

syncMode :: Mode Action
syncMode = mode
    "sync"
    (mkAction Sync)
    "Upload files to remote server"
    listFilesArg
    [targetFlag, verboseFlag, veryVerboseFlag]

pumpMode :: Mode Action
pumpMode = mode
    "pump"
    (mkAction Pump)
    "Continuously pump files to server"
    listFilesArg
    []

promoteMode :: Mode Action
promoteMode = mode
    "promote"
    (mkAction Promote)
    "Schedule entries to appear on front page"
    listFilesArg
    [targetFlag]

compositeMode :: Mode Action
compositeMode = modes
    "antisync"
    (mkAction DoNothing)
    "Command-line utility to synchronize antiblog posts"
    [statusMode, syncMode, pumpMode, promoteMode]

decideAction :: IO Action
decideAction = processArgs compositeMode

helpMessage :: String
helpMessage = show compositeMode
