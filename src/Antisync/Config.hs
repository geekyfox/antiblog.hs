
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import System.FilePath.Posix(combine)

import Skulk.Deep
import Skulk.Outcome

import Common.Config
import Utils.Data.Tagged

-- | Typesafe wrapper around the name of the system.
newtype SystemName = SystemName String deriving (Show, IsString, ToString, Eq)

instance FromJSON SystemName where
    parseJSON x = fromString <$> parseJSON x

-- | Information about a single endpoint.
data Endpoint = EP {
    -- | Name of a system. `antisync` uses it to mark/locate
    --   entry IDs in the text files.
    systemName   :: SystemName
    -- | Base URL of a remote system. `antisync` uses it to construct
    --   system's API endpoint.
    ,remoteUrl    :: BaseURL
    -- | Secret API key of a remote system. `antisync` uses it for
    --   authorization.
    ,remoteApiKey :: T.Text
}

instance FromJSON Endpoint where
    parseJSON (Object v) =
        EP <$> v .: "name" <*> v .: "url"  <*> v .: "apiKey"
    parseJSON _ = mzero

-- | Gets an API endpoint URL (basically, just appends \"/api\" to
-- 'baseUrl')
apiUrl :: Endpoint -> String
apiUrl = liftT (++ "api/") .  remoteUrl

-- | Whole configuration file.
type Config = [Endpoint]

justDigest :: (a -> Maybe b) -> [a] -> Either a [b]
justDigest f = go []
    where
        go acc [] = Right (reverse acc)
        go acc (x:xs) = case f x of
            Just y -> go (y:acc) xs
            Nothing -> Left x

digest :: ToString a => (a -> Maybe b) -> [a] -> Outcome  [b]
digest f xs = case justDigest f xs of
    Left msg -> Fail (toString msg)
    Right xs -> OK xs

instance ToString T.Text where
    toString = T.unpack

-- | Reads the configuration data from `~/.antisync/config.json`
readClientConfig :: IO (Outcome Config)
readClientConfig = readConfig go ".antisync/antisync.conf"
    where
        fetch :: ToString a => (a -> Maybe b) -> a -> Outcome b
        fetch f x = case f x of
            Just y -> OK y
            Nothing -> Skip (toString x)
        go k f = do
            url <- shapeshift <$> (fetch f "url")
            apiKey <- shapeshift <$> (fetch f "apiKey")
            return $ EP (shapeshift k) url apiKey

-- | Loads the config and looks up the endpoint by name.
loadNamed :: SystemName -> IO (Outcome Endpoint)
loadNamed n = readClientConfig >>== select n

-- | Looks up endpoints by name.
select :: SystemName -> [Endpoint] -> Outcome Endpoint
select n = maybe (fail msg) return . find match
    where
        msg = liftT (\s -> "Endpoint not found in config.json: " ++ s) n
        match e = n == systemName e

-- | Finds the suitable endpoint configuration and aborts the
--   execution if none is found.        
loadOrDie :: Maybe SystemName -> IO Endpoint
loadOrDie (Just n) = exposeOrDie <$> (loadNamed n)
loadOrDie Nothing = error "System name is missing"

