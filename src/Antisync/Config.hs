
{-# LANGUAGE OverloadedStrings #-}

{- |
    Runtime configuration.

    Config file must be a well-formed JSON of the following form:

@
{
    \"defaultServer\" : \"test\",
    \"servers\" : [
        {
            \"name\" : \"test\",
            \"url\"  : \"http://testhost/subdirectory\",
            \"apiKey\" : \"magical_constant\"
        },
        {
            \"name\" : \"prod\",
            \"url\"  : \"http://subdomain.testhost/\",
            \"apiKey\" : \"another_magical_constant\"
        }
    ]
}
@

and it should be located at `~/.antisync/config.json`.
-}

module Antisync.Config where

import Control.Applicative
import Control.Monad(mzero,liftM)
import Data.Aeson
import Data.List(find)
import Data.String
import qualified Data.Text as T

import Skulk.Outcome

import Common.Config
import Utils.Data.Tagged

-- | Typesafe wrapper around the name of the system.
newtype SystemName = SystemName String deriving Show

instance TaggedString SystemName where
    expose (SystemName s) = s
    wrap = SystemName

instance Eq SystemName where
    x == y = expose x == expose y
    
instance IsString SystemName where
    fromString = wrap

instance FromJSON SystemName where
    parseJSON = liftM wrap . parseJSON

-- | Information about a single endpoint.
data Endpoint = EP {
    -- | Name of a system. `antisync` uses it to mark/locate
    --   entry IDs in the text files.
      systemName   :: SystemName
    -- | Base URL of a remote system. `antisync` uses it to construct
    --   system's API endpoint.
    , remoteUrl    :: BaseURL
    -- | Secret API key of a remote system. `antisync` uses it for
    --   authorization.
    , remoteApiKey :: T.Text
}

instance FromJSON Endpoint where
    parseJSON (Object v) =
        EP <$> v .: "name"
           <*> v .: "url"
           <*> v .: "apiKey"
    parseJSON _ = mzero

-- | Gets an API endpoint URL (basically, just appends \"/api\" to
-- 'baseUrl')
apiUrl :: Endpoint -> String
apiUrl cfg = expose (remoteUrl cfg) ++ "api/"

-- | Whole configuration file.
data Config = CFG {
    -- | Default server to use
      defaultName :: SystemName
    -- | All known servers
    , servers     :: [Endpoint]
}

instance FromJSON Config where
    parseJSON (Object v) =
        CFG <$> v .: "defaultServer"
            <*> v .: "servers"
    parseJSON _ = mzero        

-- | Reads the configuration data from `~/.antisync/config.json`
readConfig :: IO (Outcome Config)
readConfig = loadHome ".antisync/config.json"

-- | Loads the config and looks up the default endpoint.
loadDefault :: IO (Outcome Endpoint)
loadDefault = liftM (>>= seek) readConfig
    where
        seek (CFG n es) = select n es

-- | Loads the config and looks up the endpoint by name.
loadNamed :: SystemName -> IO (Outcome Endpoint)
loadNamed n = liftM (>>= seek) readConfig
    where
        seek = select n . servers

-- | Looks up endpoints by name.
select :: SystemName -> [Endpoint] -> Outcome Endpoint
select n = maybe (fail msg) return . find match
    where
        msg = "Endpoint not found in config.json: " ++ expose n
        match e = n == systemName e

-- | Finds the suitable endpoint configuration and aborts the
--   execution if none is found.        
loadOrDie :: Maybe SystemName -> IO Endpoint
loadOrDie = liftM exposeOrDie . maybe loadDefault loadNamed
