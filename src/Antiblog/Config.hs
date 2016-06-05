{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Antiblog.Config where

import Control.Applicative hiding (empty)
import Data.HashMap.Strict(toList,lookup,HashMap,empty,fromList)
import Data.Ini
import Data.List(find)
import Data.String
import Data.Text(Text,unpack)
import System.Directory(getHomeDirectory,doesFileExist)
import System.FilePath.Posix((</>))

import Prelude hiding (readFile,lookup)

import Skulk.Deep
import Skulk.Outcome
import Skulk.ToString

instance ToString Text where
    toString = unpack

newtype SiteTitle = SiteTitle String deriving (ToString, IsString, Eq, Show)

-- | Typesafe wrapper around system's base URL.
newtype BaseURL = BaseURL String deriving (ToString, Show, Eq)

instance IsString BaseURL where
    fromString x
        | not (null x) && last x == '/' = BaseURL x
        | otherwise = BaseURL $ x ++ "/"

-- | Typesafe wrapper around the name of the system.
newtype SystemName = SystemName String deriving (Show, IsString, ToString, Eq)

class Config a where
    systemName :: a -> SystemName
    digest :: SystemName -> HashMap Text Text -> Outcome a
    serialize :: a -> HashMap Text Text

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
    } deriving (Show, Eq)

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
    ,dbHost :: String
    ,dbPort :: Int
    ,dbUser :: String
    ,dbPassword :: String
    ,dbDatabase :: String
    ,siteTitle :: SiteTitle
    ,hasAuthor :: Bool
    ,author :: String
    ,authorHref :: String
    ,hasPoweredBy :: Bool
    ,hasMicroTag :: Bool
    } deriving (Show, Eq)

instance Config Remote where
    systemName = remoteSystemName
    digest name m = Remote name <$> str "url" <*> raw "apiKey"
        where
            raw x = fromMaybe ("Key missing: " ++ unpack x) (x `lookup` m)
            str x = shapeshift <$> raw x
    serialize cfg = fromList $ map (\(k,f) -> (k, f cfg))
            [("url", shapeshift . remoteUrl)
            ,("apiKey", remoteApiKey)
            ]

instance Config Local where
    systemName = localSystemName
    digest name m = Local name <$>
            str "baseUrl" <*> raw "apiKey" <*> int "httpPort" <*>
            str "dbHost" <*> int "dbPort" <*> str "dbUser" <*>
            str "dbPassword" <*> str "dbDatabase" <*>
            str "siteTitle" <*> bool "hasAuthor" <*>
            str "author" <*> str "authorHref" <*>
            bool "hasPoweredBy" <*> bool "hasMicroTag"
        where
            raw x = fromMaybe ("Key missing: " ++ unpack x) (x `lookup` m)
            str x = shapeshift <$> raw x
            int x = (read . shapeshift) <$> raw x
            bool x = (("yes" ==) . toString) <$> raw x
    serialize cfg = fromList $ map (\(k,f) -> (k, f cfg))
            [("baseUrl", shapeshift . baseUrl)
            ,("apiKey", apiKey)
            ,("httpPort", fromString . show . httpPort)
            ,("dbHost", fromString . dbHost)
            ,("dbPort", fromString . show . dbPort)
            ,("dbUser", fromString . dbUser)
            ,("dbPassword", fromString . dbPassword)
            ,("dbDatabase", fromString . dbDatabase)
            ,("siteTitle", shapeshift . siteTitle)
            ,("hasAuthor", \c -> if hasAuthor c then "yes" else "no")
            ,("author", fromString . author)
            ,("authorHref", fromString . authorHref)
            ,("hasPoweredBy", \c -> if hasPoweredBy c then "yes" else "no")
            ,("hasMicroTag", \c -> if hasMicroTag c then "yes" else "no")
            ]

getClientConfigPath, getServerConfigPath :: IO FilePath
getClientConfigPath = do
    prefix <- getHomeDirectory
    return (prefix </> ".antisync.conf")

getServerConfigPath = do
    prefix <- getHomeDirectory
    return (prefix </> ".antiblog.conf")

-- | Finds the suitable endpoint configuration and aborts the
--   execution if none is found.        
loadOrDie :: (Config a) => SystemName -> FilePath -> IO a
loadOrDie sys filename = eject exposeOrDie $
    readBundle filename >>= wrap . select filename sys

-- | Looks up endpoints by name.
select :: (Config a) => FilePath -> SystemName -> [a] -> Outcome a
select filename sys cfgs = case findConfig sys cfgs of
    Just cfg -> OK cfg
    Nothing -> Fail ("Endpoint '" ++ toString sys ++ "' not found in file '" ++ filename ++ "'")

findConfig :: (Config a) => SystemName -> [a] -> Maybe a
findConfig sys = find ((==) sys . systemName)

digestBundle :: (Config a) => Ini -> Outcome [a]
digestBundle = mapM go . toList . unIni
    where
        go (name, values) = digest (shapeshift name) values

readConfigFile :: FilePath -> OutcomeIO Ini
readConfigFile path = Deep $ do
    ex <- doesFileExist path
    if ex then fromEither <$> readIniFile path else return $ OK $ Ini empty

readBundle :: (Config a) => FilePath -> OutcomeIO [a]
readBundle path = do
    ini <- readConfigFile path
    wrap (digestBundle ini)

serializeBundle :: (Config a) => [a] -> Ini
serializeBundle = Ini . fromList . map go
    where
        go c = (shapeshift $ systemName c, serialize c)

writeBundle :: (Config a) => FilePath -> [a] -> IO ()
writeBundle path cfg = writeIniFile path (serializeBundle cfg)
