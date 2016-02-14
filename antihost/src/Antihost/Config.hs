
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
module Antihost.Config where

import Control.Applicative
import Control.Monad(mzero,liftM)
import Data.Aeson
import qualified Data.Text as T

import Anticore.Config
import Anticore.Utils(exposeOrDie,(|>>),TaggedString(expose,wrap))

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
serverConfig fp = loadHome fp |>> exposeOrDie
