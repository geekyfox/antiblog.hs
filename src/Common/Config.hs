
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Common.Config where

import Control.Applicative
import Control.Monad(join,mzero,liftM)
import Data.Aeson
import Data.String(IsString,fromString)
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict(toList,lookup)
import Data.Ini(Ini,parseIni,unIni,readIniFile)
import qualified Data.Text as T
import Data.Text.IO(readFile)

import System.Directory(getHomeDirectory,doesFileExist)
import System.FilePath.Posix(combine)

import Skulk.Deep
import Skulk.Outcome

import Utils.Data.Tagged

import Prelude hiding (readFile,lookup)

-- | Typesafe wrapper around system's base URL.
newtype BaseURL = BaseURL String deriving ToString

instance IsString BaseURL where
    fromString x
        | (not $ null x) && (last x) == '/' = BaseURL x
        | otherwise = BaseURL $ x ++ "/"

instance FromJSON BaseURL where
    parseJSON x = fromString <$> parseJSON x

-- | Loads settings from a file
load :: (FromJSON a) => FilePath -> IO (Outcome a)
load fname = (eitherToProc . eitherDecode) <$> L.readFile fname
    where
        eitherToProc (Left errmsg) = fail $ "Error parsing '" ++ fname ++ "': " ++ errmsg
        eitherToProc (Right v) = return v

-- | Loads settings from ~\/\</filename/\>
loadHome :: (FromJSON a) => FilePath -> IO (Outcome a)
loadHome suffix = do
    prefix <- getHomeDirectory
    load $ combine prefix suffix

fromEither :: Either String a -> Outcome a
fromEither (Left msg) = Fail msg
fromEither (Right v) = OK v

readConfig :: (T.Text -> (T.Text -> Maybe T.Text) -> Outcome a) -> FilePath -> IO (Outcome [a])
readConfig conv suffix = do
    prefix <- getHomeDirectory
    let fname = combine prefix suffix
    ex <- doesFileExist fname
    if ex
        then do
            let ini = fromEither <$> (readIniFile fname)
            ini >>== (parseConfig conv)
        else
            return (OK [])
    
parseConfig :: (T.Text -> (T.Text -> Maybe T.Text) -> Outcome a) -> Ini -> Outcome [a]        
parseConfig conv = allOK . map go . toList . unIni
    where
        go (k, v) = conv k (flip lookup v)

newtype SiteTitle = SiteTitle String deriving (ToString, IsString)

instance FromJSON SiteTitle where
    parseJSON x = fromString <$> parseJSON x

-- | Runtime configuration for the server.
data ConfigSRV = SRV {
    -- | Base URL of a system. Used by `antiblog` as root for local
    --   hyperlinks.
     baseUrl    :: BaseURL
    -- | Secret API key.
    ,apiKey     :: T.Text
    -- | TCP port of webserver.
    ,httpPort   :: Int
    -- | Database connection string.
    ,dbConnString :: String
    ,siteTitle :: SiteTitle
    ,hasAuthor :: Bool
    ,hasPoweredBy :: Bool
    ,hasMicroTag :: Bool
}

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

-- | Loads server settings from `~/<filename>`.
serverConfig :: FilePath -> IO ConfigSRV
serverConfig fp = exposeOrDie <$> load fp
