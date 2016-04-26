{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Antiblog.Config where

import Control.Applicative
import Control.Arrow((***))
import Data.HashMap.Strict(toList,lookup)
import Data.Ini(Ini,unIni,readIniFile)
import Data.List(find)
import Data.String
import Data.Text(Text,unpack)
import Data.Traversable
import System.Directory(getHomeDirectory,doesFileExist)
import System.FilePath.Posix(combine)

import Prelude hiding (readFile,lookup)

import Skulk.Deep
import Skulk.Outcome
import Skulk.ToString

instance ToString Text where
    toString = unpack

newtype SiteTitle = SiteTitle String deriving (ToString, IsString)

-- | Typesafe wrapper around system's base URL.
newtype BaseURL = BaseURL String deriving ToString

instance IsString BaseURL where
    fromString x
        | not (null x) && last x == '/' = BaseURL x
        | otherwise = BaseURL $ x ++ "/"

-- | Typesafe wrapper around the name of the system.
newtype SystemName = SystemName String deriving (Show, IsString, ToString, Eq)

class Config a where
    systemName :: a -> SystemName
    load :: IO (Outcome [a])
    digest :: (SystemName, Text -> Outcome Text) -> Outcome a

-- | Information about a single endpoint.
data Remote = Remote {
    -- | Name of a system. `antisync` uses it to mark/locate
    --   entry IDs in the text files.
    remoteSystemName :: SystemName
    -- | Base URL of a remote system. `antisync` uses it to construct
    --   system's API endpoint.
    ,remoteUrl :: BaseURL
    -- | Secret API key of a remote system. `antisync` uses it for
    --   authorization.
    ,remoteApiKey :: Text
    }

-- | Runtime configuration for the server.
data Local = Local {
    localSystemName :: SystemName
    -- | Base URL of a system. Used by `antiblog` as root for local
    --   hyperlinks.
    ,baseUrl :: BaseURL
    -- | Secret API key.
    ,apiKey :: Text
    -- | TCP port of webserver.
    ,httpPort :: Int
    -- | Database connection string.
    ,dbConnString :: String
    ,siteTitle :: SiteTitle
    ,hasAuthor :: Bool
    ,author :: String
    ,authorHref :: String
    ,hasPoweredBy :: Bool
    ,hasMicroTag :: Bool
    }

instance Config Remote where
    systemName = remoteSystemName
    load = readConfig ".antisync/antisync.conf" >>== (sequenceA . map digest)
    digest (name, raw) = Remote name <$> str "url" <*> raw "apiKey"
        where
            str x = shapeshift <$> raw x

instance Config Local where
    systemName = localSystemName
    load = readConfig ".antiblog/antiblog.conf" >>== (sequenceA . map digest)
    digest (name, raw) = Local name <$>
            str "baseUrl" <*> raw "apiKey" <*> int "httpPort" <*>
            str "dbConnString" <*> str "siteTitle" <*> bool "hasAuthor" <*>
            str "author" <*> str "authorHref" <*>
            bool "hasPoweredBy" <*> bool "hasMicroTag"
        where
            str x = shapeshift <$> raw x
            int x = (read . shapeshift) <$> raw x
            bool x = (("yes" ==) . toString) <$> raw x

-- | Finds the suitable endpoint configuration and aborts the
--   execution if none is found.        
loadOrDie :: (Config a) => Maybe SystemName -> IO a
loadOrDie n = case n of
    Just n -> exposeOrDie <$> (load >>== select n)
    Nothing -> error "System name is missing"

-- | Looks up endpoints by name.
select :: (Config a) => SystemName -> [a] -> Outcome a
select n = maybe (fail msg) return . find match
    where
        msg = "Endpoint not found in config.json: " ++ toString n
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
