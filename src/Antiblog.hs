
-- | Entrypoint module of `antiblog` executable.
module Main(main) where

import Control.Monad(when)
import System.Environment(getArgs)
import System.IO
import Web.Scotty

import Antiblog.Config
import Antiblog.Database
import Antiblog.Routing

-- | Entrypoint.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    when (length args /= 1) (error "System name is missing")
    let sysName = SystemName (head args)
    cfg <- getServerConfigPath >>= loadOrDie sysName
    db <- mkConnPool cfg
    scotty (httpPort cfg) $ routing db cfg
