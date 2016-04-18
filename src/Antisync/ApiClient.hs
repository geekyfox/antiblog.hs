{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HTTP API client.
--
--   All API calls need `ConfigCLI` structure to 

module Antisync.ApiClient where

import Control.Applicative
import Control.Arrow((***))
import Control.Exception(catch)
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Control.Monad(liftM)
import Data.Aeson
import Data.Aeson.Types(Pair)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8(pack, unpack)
import Data.CaseInsensitive(mk)
import Data.Map(Map, fromList)
import Data.Maybe(catMaybes)
import Data.Text.Encoding

import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

import Skulk.Outcome

import Common.Api
import Common.Model
import Utils.Data.Tagged

import Antiblog.Config

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

-- | Gets an API endpoint URL (basically, just appends \"/api\" to
-- 'baseUrl')
apiUrl :: Remote -> String
apiUrl = liftT (++ "api/") .  remoteUrl

-- | Prepares a request for querying specific API method.
mkRequest :: String -> Remote -> IO Request
mkRequest method sys = setQueryString qs <$> parseUrl url
    where
        url = apiUrl sys ++ method
        qs  = [("api_key", Just $ encodeUtf8 $ remoteApiKey sys)]

mkJsonPostRequest :: String -> Remote -> Value -> IO Request
mkJsonPostRequest method sys payload = attachArgs <$> baseRequest
    where
        baseRequest = mkRequest method sys
        encodedArgs  = [("payload", B.pack $ LB.unpack $ encode payload)]
        attachArgs  = urlEncodedBody encodedArgs

-- | Prepares a request for POSTing to specific API method.
mkPostRequest :: String -> Remote -> [(String, String)] -> IO Request
mkPostRequest method sys args = attachArgs <$> baseRequest
    where
        baseRequest = mkRequest method sys
        encodedArgs = map (pack *** pack) args
        attachArgs  = urlEncodedBody encodedArgs

-- | Retrieves the list of entries on the server.
retrieveIndex :: Remote -> IO (Outcome [StoredId])
retrieveIndex sys = mkRequest "index" sys >>= query

-- | Shorthand type.
type EntryIndex = Map Int MD5Sig

-- | Retrieves the list of entries on server as `Map Int String`.
queryIndexAsMap :: Remote -> IO (Outcome EntryIndex)
queryIndexAsMap sys = fmap toMap <$> retrieveIndex sys
    where
        toMap es = fromList [ (a,b) | StoredId a b <- es ]

-- | Gathers arguments for create/update POST request.
encodeEntryImpl :: Entry a StoredContent StoredExtra -> [Pair]
encodeEntryImpl e = [
    "body" .= body e
    ,"tags" .= tags e
    ,"series" .= seriesRef e
    ] ++ catMaybes [
        (\v -> "title" .= v) <$> emptyToNothing (title e)
        ,(\v -> "summary" .= v) <$> emptyToNothing (summary e)
        ,(\v -> "symlink" .= v) <$> symlink e
        ,(\v -> "metalink" .= v) <$> metalink e
        ]

encodeNewEntry :: NewEntry -> Value
encodeNewEntry e = object $ ("signature" .= md5sig e):encodeEntryImpl e

encodeStoredEntry :: StoredEntry -> Value
encodeStoredEntry e = object $ ("signature" .= md5sig e):("id" .= entryId e):encodeEntryImpl e

encodeRedirect :: EntryRedirect -> Value
encodeRedirect r = object $
    ["id" .= entryId r, "url" .= permalink r, "signature" .= permalink r] ++ catMaybes [
        (\txt -> "symlink" .= txt) <$> symlink r
        ,(\txt -> "metalink" .= txt) <$> metalink r
        ]

-- | Updates an entry.
updateEntry :: Remote -> StoredEntry -> IO (Outcome ReplyUP)
updateEntry sys e = mkJsonPostRequest "update" sys (encodeStoredEntry e) >>= query

-- | Creates an entry, returning new entry's ID on success.
createEntry :: Remote -> NewEntry -> IO (Outcome ReplyCR)
createEntry sys e = mkJsonPostRequest "create" sys (encodeNewEntry e) >>= query

promoteEntry :: Remote -> Int -> IO (Outcome ReplyPR)
promoteEntry sys uid = mkPostRequest "promote" sys params >>= query
    where
        params = [("id", show uid)]

updateRedirect :: Remote -> EntryRedirect -> IO (Outcome ReplyUP)
updateRedirect sys e = mkJsonPostRequest "redirect" sys (encodeRedirect e) >>= query
