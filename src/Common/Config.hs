
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

module Common.Config where

import Control.Applicative
import Control.Monad(mzero,liftM)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import System.Directory(getHomeDirectory)
import System.FilePath.Posix(combine)

import Skulk.Outcome

import Utils.Data.Tagged

-- | Typesafe wrapper around system's base URL.
newtype BaseURL = BaseURL String

instance TaggedString BaseURL where
    expose (BaseURL s) = s
    wrap x
        | last x == '/' = BaseURL x
        | otherwise     = BaseURL $ x ++ "/"

instance FromJSON BaseURL where
    parseJSON = liftM wrap . parseJSON

-- | Loads settings from a file
load :: (FromJSON a) => FilePath -> IO (Outcome a)
load fname = eitherToProc <$> eitherDecode <$> L.readFile fname
    where eitherToProc (Left errmsg) = fail $
            "Error parsing '" ++ fname ++ "': " ++ errmsg
          eitherToProc (Right v) = return v

-- | Loads settings from ~\/\</filename/\>
loadHome :: (FromJSON a) => FilePath -> IO (Outcome a)
loadHome suffix = do
    prefix <- getHomeDirectory
    load $ combine prefix suffix

newtype SiteTitle = SiteTitle String

instance TaggedString SiteTitle where
    expose (SiteTitle s) = s
    wrap = SiteTitle

instance FromJSON SiteTitle where
    parseJSON = liftM wrap . parseJSON

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
serverConfig fp = exposeOrDie <$> loadHome fp
