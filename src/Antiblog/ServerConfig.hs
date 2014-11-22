
{-# LANGUAGE OverloadedStrings #-}

{- |
    Runtime configuration.

    Config file must be a well-formed JSON of the following form:

@
{
    \"url\"      \: \"http://hostname/subdirectory\",
    \"apiKey\"   \: \"secure_magic_constant\",
    \"httpPort\" \: 3000,
    \"dbConn\"   \: \"host\=\'localhost\' port\=5432 user\=user dbname\=\'dbname\' etc\"
}
@
-}
module Antiblog.ServerConfig where

import Control.Applicative
import Control.Monad(mzero)
import Data.Aeson
import qualified Data.Text as T

import Anticommon.Config
import Utils(exposeOrDie,(|>>))

-- | Runtime configuration for the server.
data ConfigSRV = SRV {
    -- | Base URL of a system. Used by `antiblog` as root for local
    --   hyperlinks.
    baseUrl    :: BaseURL,
    -- | Secret API key.
    apiKey     :: T.Text,
    -- | TCP port of webserver.
    httpPort   :: Int,
    -- | Database connection string.
    dbConnString :: String
}

instance FromJSON ConfigSRV where
    parseJSON (Object v) =
        SRV <$> v .: "url"
            <*> v .: "apiKey"
            <*> v .: "httpPort"
            <*> v .: "dbConn"
    parseJSON _ = mzero

-- | Loads server settings from `~/<filename>`.
serverConfig :: FilePath -> IO ConfigSRV
serverConfig fp = loadHome fp |>> exposeOrDie
