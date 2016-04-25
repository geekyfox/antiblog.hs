
module Main(main) where

import Control.Monad(when)
import Data.String
import System.Console.CmdArgs.Explicit
import System.Environment(getArgs)

import Antiblog.Config
import Common.Database

type Task = Maybe SystemName -> IO ()

data Action = Action {
    invoke :: Task
    ,endpoint :: Maybe SystemName
    }

systemArg :: Arg Action
systemArg = Arg
    {argValue = \v a -> Right $ a { endpoint = Just $ fromString v }
    ,argType = "<name of website configuration>"
    ,argRequire = True
    }

mkAction :: Task -> Action
mkAction task = Action task Nothing

mkMode :: Name -> Task -> Help -> Bool -> Mode Action
mkMode name task help hasSystem = mode
        {modeNames = [name]
        ,modeHelp = help
        ,modeArgs = args
        ,modeGroupFlags = toGroup []
        }
    where
        mode = modeEmpty (mkAction task)
        args
            | hasSystem = ([systemArg], Nothing)
            | otherwise = ([], Nothing)

rotateMode :: Mode Action
rotateMode = mkMode "rotate" go help True
    where
        go sys = loadOrDie sys
            >>= connect . dbConnString
            >>= rotateEntries
            >>= mapM_ putStrLn
        help = "Rotates entries"

helpAction :: Action
helpAction = mkAction (\_ -> print compositeMode)

compositeMode :: Mode Action
compositeMode = modes
    "antiwork"
    helpAction
    "Antiblog's backend tasks"
    [rotateMode]

decideAction :: IO Action
decideAction = processArgs compositeMode

main :: IO ()
main = do
    a <- decideAction
    invoke a (endpoint a)
