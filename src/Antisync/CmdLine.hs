
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

import Data.String(fromString)
import System.Console.CmdArgs.Explicit
import Skulk.Outcome
import Utils.Data.Tagged
import Antiblog.Config(SystemName)

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
     actionType :: ActionType
    ,actionEndpoint :: Maybe SystemName
    ,actionVerbosity :: Verbosity
    ,actionFiles :: [String]
}

mkAction :: ActionType -> Action
mkAction t = Action t Nothing Normal []

verboseFlag :: Flag Action
verboseFlag = flagNone ["verbose"]
        (\a -> a { actionVerbosity = Verbose })
        "Print skipped entries (except not modified ones)"

veryVerboseFlag :: Flag Action
veryVerboseFlag = flagNone ["very-verbose"]
        (\a -> a { actionVerbosity = VeryVerbose })
        "Print all skipped entries"

targetArg :: Arg Action
targetArg = Arg
    {argValue = \v a -> Right $ a { actionEndpoint = Just $ fromString v }
    ,argType = "<name of remote endpoint>"
    ,argRequire = True
    }

listFilesArg :: Arg Action
listFilesArg = Arg
    {argValue = \v a -> Right $ a { actionFiles = v:actionFiles a}
    ,argType = "<files to upload>"
    ,argRequire = True
    }

command :: ActionType -> Mode Action
command x =
    (modeEmpty (mkAction x))
        {modeNames = [actionName x]
        ,modeHelp = actionHelp x
        ,modeArgs = actionArgs x
        ,modeGroupFlags = toGroup (actionFlags x)
        }

actionName :: ActionType -> Name
actionName Status = "status"
actionName Sync = "sync"
actionName Pump = "pump"
actionName Promote = "promote"

actionHelp :: ActionType -> Help
actionHelp Status = "Compare status of local files to remote server"
actionHelp Sync = "Upload files to remote server"
actionHelp Pump = "Continuously pump files to server"
actionHelp Promote = "Schedule entries to appear on front page"

actionArgs :: ActionType -> ([Arg Action], Maybe (Arg Action))
actionArgs Status = ([targetArg], Nothing)
actionArgs Sync = ([targetArg], Just listFilesArg)
actionArgs Pump = ([targetArg], Just listFilesArg)
actionArgs Promote = ([targetArg], Just listFilesArg)

actionFlags :: ActionType -> [Flag Action]
actionFlags Status = [verboseFlag, veryVerboseFlag]
actionFlags Sync = [verboseFlag, veryVerboseFlag]
actionFlags Pump = []
actionFlags Promote = []

compositeMode :: Mode Action
compositeMode = modes
    "antisync"
    (mkAction DoNothing)
    "Command-line utility to synchronize antiblog posts"
    (map command [Status, Sync, Pump, Promote])

decideAction :: IO Action
decideAction = processArgs compositeMode

helpMessage :: String
helpMessage = show compositeMode
