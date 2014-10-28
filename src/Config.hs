
{-# LANGUAGE OverloadedStrings #-}

{- |
    Runtime configuration.

    Config file must be a well-formed JSON of the following form:

@
{
    \"name\"     \: \"dev\",
    \"url\"      \: \"http://hostname/subdirectory\",
    \"apiKey\"   \: \"secure_magic_constant\",
    \"httpPort\" \: 3000,
    \"dbConn\"   \: \"host\=\'localhost\' port\=5432 user\=user dbname\=\'dbname\' etc\"
}
@

Mandatory fields for client-side config are: \"name\", \"url\" and
\"apiKey\". Mandatory fields for server-side config are: \"url\",
\"apiKey\", \"httpPort\" and \"dbConn\".
-}
module Config where

import Control.Monad(mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.Directory(getHomeDirectory)
import System.FilePath.Posix(combine)

import Utils

-- | Typesafe wrapper around the name of the system.
newtype SystemName = SystemName String deriving Show

instance TaggedString SystemName where
    expose (SystemName s) = s
    wrap = SystemName

-- | Typesafe wrapper around system's base URL.
newtype BaseURL = BaseURL String

instance TaggedString BaseURL where
    expose (BaseURL s) = s
    wrap = BaseURL

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

-- | Configuration for the client.
data ConfigCLI = CLI {
    -- | Name of a system. `antisync` uses it to mark/locate
    --   entry IDs in the text files.
    systemName   :: SystemName,
    -- | Base URL of a remote system. `antisync` uses it to construct
    --   system's API endpoint.
    remoteUrl    :: BaseURL,
    -- | Secret API key of a remote system. `antisync` uses it for
    --   authorization.
    remoteApiKey :: T.Text
}

-- | Gets an API endpoint URL (basically, just appends \"/api\" to
-- 'baseUrl')
apiUrl :: ConfigCLI -> String
apiUrl cfg = expose (remoteUrl cfg) ++ "api/"

mkurl :: String -> BaseURL
mkurl x
  | last x == '/' = wrap x
  | otherwise     = wrap $ x ++ "/"

instance FromJSON ConfigSRV where
    parseJSON (Object v) = do
        url    <- v .: "url"
        key    <- v .: "apiKey"
        port   <- v .: "httpPort"
        dbConn <- v .: "dbConn"
        return SRV {
            baseUrl      = mkurl url,
            apiKey       = key,
            httpPort     = port,
            dbConnString = dbConn
        }
    parseJSON _ = mzero

instance FromJSON ConfigCLI where
    parseJSON (Object v) = do
        name <- v .: "name"
        url  <- v .: "url"
        key  <- v .: "apiKey"
        return CLI {
            systemName   = wrap name,
            remoteUrl    = mkurl url,
            remoteApiKey = key
        }
    parseJSON _ = mzero

-- | Loads settings from a file
load :: (FromJSON a) => FilePath -> IO a
load fname = L.readFile fname |>> eitherDecode |>> exposeOrDie
    where exposeOrDie (Left errmsg) = error $
            "Error parsing '" ++ fname ++ "': " ++ errmsg
          exposeOrDie (Right v)     = v

-- | Loads settings from ~\/\</filename/\>
loadHome :: (FromJSON a) => FilePath -> IO a
loadHome suffix = do
    prefix <- getHomeDirectory
    load $ combine prefix suffix

-- | Loads server settings from ~\/antiblog\/config.json
sysDefault :: IO ConfigSRV
sysDefault = loadHome "antiblog/config.json"

-- | Loads client settings from ~\/antiblog\/config-dev.json
sysDev :: IO ConfigCLI
sysDev = loadHome "antiblog/config-dev.json"

-- | Loads client settings from ~\/antiblog\/config-prod.json
sysProd :: IO ConfigCLI
sysProd = loadHome "antiblog/config-prod.json"


