
module Main(main) where

import Control.Monad(when)
import System.Environment(getArgs)

import Antihost.Config
import Antihost.Database

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (error "Config file location is missing")
    sys <- serverConfig (head args)
    db <- mkPool (dbConnString sys)
    rotateEntries db >>= mapM_ putStrLn
