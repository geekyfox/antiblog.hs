{-# LANGUAGE OverloadedStrings #-}

-- | HTTP API client.
--
--   All API calls need `ConfigCLI` structure to 

module ApiClient where

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

import Api
import Config
import Model
import Utils

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
query :: (FromJSON a) => Request -> IO (Processed a)
query req = withManager defaultManagerSettings protect |>> decodeData
    where
        protect mgr  = work mgr `catch` handleError
        work mgr     = withResponse req mgr handleResult
        handleResult = liftM OK . brConsume . responseBody
        handleError  = return . Fail . fmtHttpError

-- | Prepares a request for querying specific API method.
mkRequest :: String -> ConfigCLI -> IO Request
mkRequest method sys = parseUrl url |>> setQueryString qs
    where
        url = apiUrl sys ++ method
        qs  = [("api_key", Just $ encodeUtf8 $ remoteApiKey sys)]

-- | Prepares a request for POSTing to specific API method.
mkPostRequest :: String -> ConfigCLI -> [(String, String)] -> IO Request
mkPostRequest method sys args = baseRequest |>> attachArgs
    where
        baseRequest = mkRequest method sys
        encodedArgs = map (pack *** pack) args
        attachArgs  = urlEncodedBody encodedArgs

-- | Retrieves the list of entries on the server.
retrieveIndex :: ConfigCLI -> IOProc [EntryHash]
retrieveIndex sys = mkRequest "index" sys >>= query

-- | Shorthand type.
type EntryIndex = Map Int String

-- | Retrieves the list of entries on server as `Map Int String`.
queryIndexAsMap :: ConfigCLI -> IOProc EntryIndex
queryIndexAsMap sys = retrieveIndex sys |>> fmap toMap
    where
        toMap es = fromList [ (huid e, hash e) | e <- es ]

-- | Gathers arguments for create/update POST request.
encode :: EntryFS -> [(String, String)]
encode e = mapMaybe wrap optionals ++ mandatories
    where
        get fun      = fun e |>> expose
        wrap (k, mv) = maybe Nothing (\v -> Just (k, v)) mv
        optionals =
            [ ("id",        uid e |>> show)
            , ("title",     get title)
            , ("summary",   get summary)
            , ("symlink",   get symlink)
            , ("metalink",  get metalink)
            ]
        mandatories =
            [ ("signature", md5sig e)
            , ("body",      expose $ body e)
            , ("tags",      expose $ tags e)
            ]

-- | Updates an entry.
updateEntry :: ConfigCLI -> EntryFS -> IOProc ReplyUP
updateEntry sys e = mkPostRequest "update" sys (encode e) >>= query

-- | Creates an entry, returning new entry's ID on success.
createEntry :: ConfigCLI -> EntryFS -> IOProc ReplyCR
createEntry sys e = mkPostRequest "create" sys (encode e) >>= query
