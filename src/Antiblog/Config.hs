{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Antiblog.Config where

import Control.Applicative
import Control.Arrow((***))
import Control.Monad(join,mzero,liftM)
import Data.Aeson
import Data.ByteString.Lazy(readFile)
import Data.HashMap.Strict(HashMap,toList,lookup)
import Data.Ini(Ini,parseIni,unIni,readIniFile)
import Data.List(find)
import Data.String
import Data.Text(Text,unpack)
import Data.Traversable
import System.Directory(getHomeDirectory,doesFileExist)
import System.FilePath.Posix(combine)


import Prelude hiding (readFile,lookup)

import Skulk.Deep
import Skulk.Outcome

import Utils.Data.Tagged

instance ToString Text where
    toString = unpack

newtype SiteTitle = SiteTitle String deriving (ToString, IsString)

instance FromJSON SiteTitle where
    parseJSON x = fromString <$> parseJSON x

-- | Typesafe wrapper around system's base URL.
newtype BaseURL = BaseURL String deriving ToString

instance IsString BaseURL where
    fromString x
        | not (null x) && last x == '/' = BaseURL x
        | otherwise = BaseURL $ x ++ "/"

instance FromJSON BaseURL where
    parseJSON x = fromString <$> parseJSON x

-- | Typesafe wrapper around the name of the system.
newtype SystemName = SystemName String deriving (Show, IsString, ToString, Eq)

-- | Runtime configuration for the server.
data ConfigSRV = SRV {
    -- | Base URL of a system. Used by `antiblog` as root for local
    --   hyperlinks.
     baseUrl :: BaseURL
    -- | Secret API key.
    ,apiKey :: Text
    -- | TCP port of webserver.
    ,httpPort :: Int
    -- | Database connection string.
    ,dbConnString :: String
    ,siteTitle :: SiteTitle
    ,hasAuthor :: Bool
    ,hasPoweredBy :: Bool
    ,hasMicroTag :: Bool
    }

-- | Information about a single endpoint.
data Endpoint = EP {
    -- | Name of a system. `antisync` uses it to mark/locate
    --   entry IDs in the text files.
    systemName :: SystemName
    -- | Base URL of a remote system. `antisync` uses it to construct
    --   system's API endpoint.
    ,remoteUrl :: BaseURL
    -- | Secret API key of a remote system. `antisync` uses it for
    --   authorization.
    ,remoteApiKey :: Text
    }

-- | Whole configuration file.
type Config = [Endpoint]

-- | Gets an API endpoint URL (basically, just appends \"/api\" to
-- 'baseUrl')
apiUrl :: Endpoint -> String
apiUrl = liftT (++ "api/") .  remoteUrl

instance FromJSON ConfigSRV where
    parseJSON (Object v) =
        SRV <$> v .: "url"
            <*> v .: "apiKey"
            <*> v .: "httpPort"
            <*> v .: "dbConn"
            <*> v .: "siteTitle"
            <*> v .: "hasAuthor"
            <*> v .: "hasPoweredBy"
            <*> v .: "hasMicroTag"
    parseJSON _ = mzero

-- | Loads settings from a file
load :: (FromJSON a) => FilePath -> IO (Outcome a)
load fname = (eitherToProc . eitherDecode) <$> readFile fname
    where
        eitherToProc (Left errmsg) = fail $ "Error parsing '" ++ fname ++ "': " ++ errmsg
        eitherToProc (Right v) = return v

-- | Loads server settings from `~/<filename>`.
serverConfig :: FilePath -> IO ConfigSRV
serverConfig fp = exposeOrDie <$> load fp

-- | Finds the suitable endpoint configuration and aborts the
--   execution if none is found.        
loadOrDie :: Maybe SystemName -> IO Endpoint
loadOrDie (Just n) = exposeOrDie <$> loadNamed n
loadOrDie Nothing = error "System name is missing"

-- | Loads the config and looks up the endpoint by name.
loadNamed :: SystemName -> IO (Outcome Endpoint)
loadNamed n = readClientConfig >>== select n

-- | Reads the configuration data from `~/.antisync/antisync.conf`
readClientConfig :: IO (Outcome Config)
readClientConfig = readConfig ".antisync/antisync.conf" >>== (sequenceA . map digest)
    where
        digest :: (SystemName, Text -> Outcome Text) -> Outcome Endpoint
        digest (name, raw) =
            let str x = shapeshift <$> raw x
            in EP name <$> str "url" <*> raw "apiKey"

-- | Looks up endpoints by name.
select :: SystemName -> [Endpoint] -> Outcome Endpoint
select n = maybe (fail msg) return . find match
    where
        msg = liftT (\s -> "Endpoint not found in config.json: " ++ s) n
        match e = n == systemName e

fromEither :: Either String a -> Outcome a
fromEither (Left msg) = Fail msg
fromEither (Right v) = OK v

readConfig :: FilePath -> IO (Outcome [(SystemName, Text -> Outcome Text)])
readConfig suffix = do
    prefix <- getHomeDirectory
    let fname = combine prefix suffix
    ex <- doesFileExist fname
    if ex
        then
            parseConfig <$$> fromEither <$> readIniFile fname
        else
            return (OK [])

fromMaybe :: String -> Maybe a -> Outcome a
fromMaybe msg = maybe (Fail msg) OK

parseConfig :: Ini -> [(SystemName, Text -> Outcome Text)]
parseConfig = map (shapeshift *** impl) . toList . unIni
    where
        impl v x = fromMaybe ("Key missing: " ++ unpack x) (x `lookup` v)
