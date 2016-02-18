{-# LANGUAGE OverloadedStrings #-}

-- | HTTP API client.
--
--   All API calls need `ConfigCLI` structure to 

module Antisync.ApiClient where

import Control.Applicative
import Control.Arrow((***))
import Control.Exception(catch)
import Control.Monad(liftM)
import Data.Aeson(FromJSON)
import Data.ByteString.Char8(pack, unpack)
import Data.CaseInsensitive(mk)
import Data.Map(Map, fromList)
import Data.Maybe(mapMaybe)
import Data.Text.Encoding

import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

import Anticore.Api
import Anticore.Data.Outcome
import Anticore.Data.Tagged
import Anticore.Model

import Antisync.Config

-- | Picks headers with specific name from a header list.
fetchHeaders :: String -> ResponseHeaders -> [String]
fetchHeaders name = map (unpack . snd) . filter ((==) key . fst)
    where
        key = mk $ pack name

-- | Somewhat more human-readable format of `HttpException`.
fmtHttpError :: HttpException -> String
fmtHttpError (StatusCodeException s hs _) = concat $
    [show $ statusCode s, "; ", unpack $ statusMessage s, "; "] ++
    fetchHeaders "x-response-body-start" hs
fmtHttpError ex = show ex

-- | Main wrapper for API calls. Makes a request, then parses response
--   using `decodeData`.
query :: (FromJSON a) => Request -> IO (Outcome a)
query req = decode <$> withManager defaultManagerSettings protect
    where
        protect mgr  = work mgr `catch` handleError
        work mgr     = withResponse req mgr handleResult
        handleResult = liftM OK . brConsume . responseBody
        handleError  = return . Fail . fmtHttpError
        decode x     = x >>= decodeData

-- | Prepares a request for querying specific API method.
mkRequest :: String -> Endpoint -> IO Request
mkRequest method sys = setQueryString qs <$> parseUrl url
    where
        url = apiUrl sys ++ method
        qs  = [("api_key", Just $ encodeUtf8 $ remoteApiKey sys)]

-- | Prepares a request for POSTing to specific API method.
mkPostRequest :: String -> Endpoint -> [(String, String)] -> IO Request
mkPostRequest method sys args = attachArgs <$> baseRequest
    where
        baseRequest = mkRequest method sys
        encodedArgs = map (pack *** pack) args
        attachArgs  = urlEncodedBody encodedArgs

-- | Retrieves the list of entries on the server.
retrieveIndex :: Endpoint -> IO (Outcome [EntryHash])
retrieveIndex sys = mkRequest "index" sys >>= query

-- | Shorthand type.
type EntryIndex = Map Int String

-- | Retrieves the list of entries on server as `Map Int String`.
queryIndexAsMap :: Endpoint -> IO (Outcome EntryIndex)
queryIndexAsMap sys = fmap toMap <$> retrieveIndex sys
    where
        toMap es = fromList [ (huid e, hash e) | e <- es ]

-- | Gathers arguments for create/update POST request.
encodeEntry :: EntryFS -> [(String, String)]
encodeEntry e = mapMaybe wrap optionals ++ mandatories
    where
        get fun      = expose <$> fun e
        wrap (k, mv) = maybe Nothing (\v -> Just (k, v)) mv
        optionals =
            [ ("id", show <$> uid e)
            , ("title",     get title)
            , ("summary",   get summary)
            , ("symlink",   get symlink)
            , ("metalink",  get metalink)
            ]
        mandatories =
            [ ("signature", md5sig e)
            , ("body",      expose $ body e)
            , ("tags",      expose $ tags e)
            , ("series",    encode $ seriesRef e)
            ]

-- | Updates an entry.
updateEntry :: Endpoint -> EntryFS -> IO (Outcome ReplyUP)
updateEntry sys e = mkPostRequest "update" sys (encodeEntry e) >>= query

-- | Creates an entry, returning new entry's ID on success.
createEntry :: Endpoint -> EntryFS -> IO (Outcome ReplyCR)
createEntry sys e = mkPostRequest "create" sys (encodeEntry e) >>= query

promoteEntry :: Endpoint -> Int -> IO (Outcome ReplyPR)
promoteEntry sys uid = mkPostRequest "promote" sys params >>= query
    where
        params = [("id", show uid)]

