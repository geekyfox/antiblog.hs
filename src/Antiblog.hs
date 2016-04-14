
-- | Entrypoint module of `antiblog` executable.
module Main(main) where

import Control.Monad(when)
import System.Environment(getArgs)
import System.IO
import Web.Scotty

import Antiblog.Config
import Antiblog.Routing
import Common.Database

-- | Entrypoint.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    when (length args /= 1) (error "Config file location is missing")
    sys <- serverConfig (head args)    
    db <- connect (dbConnString sys)
    scotty (httpPort sys) $ routing db sys
