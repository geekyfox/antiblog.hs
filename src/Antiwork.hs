
module Main(main) where

import Control.Monad(when)
import System.Environment(getArgs)

import Antiblog.Config
import Common.Database

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (error "Config file location is missing")
    sys <- serverConfig (head args)
    db <- connect (dbConnString sys)
    rotateEntries db >>= mapM_ putStrLn
