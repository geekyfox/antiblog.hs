
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

-- | Data model of client-server API.

module Antiblog.Api where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Exception(catch)
import Control.Monad(liftM, mzero)
import Data.Aeson
import Data.Aeson.Types(Pair)
import Data.ByteString(ByteString)
import Data.ByteString.Char8(pack, unpack)
import Data.ByteString.Lazy(toStrict, fromChunks)
import Data.CaseInsensitive(mk)
import Data.Map(Map, fromList)
import Data.Maybe(catMaybes)
import Data.Text.Encoding(encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Skulk.Deep
import Skulk.Outcome
import Skulk.ToString

import Common.Model
import Antiblog.Config

deriving instance ToJSON Title
deriving instance ToJSON Metalink
deriving instance ToJSON Symlink
deriving instance ToJSON Summary
deriving instance ToJSON Body
deriving instance ToJSON Tag
deriving instance ToJSON MD5Sig
deriving instance ToJSON Permalink

deriving instance FromJSON Title
deriving instance FromJSON Metalink
deriving instance FromJSON Symlink
deriving instance FromJSON Summary
deriving instance FromJSON Body
deriving instance FromJSON Tag
deriving instance FromJSON MD5Sig
deriving instance FromJSON Permalink

data ApiMessage a = AM { unwrap :: a }

instance FromJSON StoredId where
    parseJSON (Object v) = StoredId <$> v .: "id" <*> v .: "signature"
    parseJSON _ = mzero

instance ToJSON StoredId where
    toJSON (StoredId uid sig) = object ["id" .= uid, "signature" .= sig]

instance (FromJSON x) => FromJSON (ApiMessage x) where
    parseJSON (Object v) = AM <$> v.: "content"
    parseJSON _ = mzero

instance (ToJSON x) => ToJSON (ApiMessage x) where
    toJSON (AM x) = object ["content" .= toJSON x]

instance ToJSON SeriesRef where
    toJSON (SeriesRef a b) = object ["series" .= a, "index" .= b]

instance FromJSON SeriesRef where
    parseJSON (Object v) = SeriesRef <$> v .: "series" <*> v .: "index"
    parseJSON _ = mzero

instance FromJSON RedirectExtra where
    parseJSON (Object v) = RedirectExtra <$> v .:? "symlink" <*> v .:? "metalink"
    parseJSON _ = mzero

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Entry a b c) where
    parseJSON v = Entry <$> parseJSON v <*> parseJSON v <*> parseJSON v

type Client = (Manager, Remote)

mkClient :: Maybe SystemName -> IO Client
mkClient name = do
    cfg <- loadOrDie name
    mgr <- newManager defaultManagerSettings
    return (mgr, cfg)

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

-- | Parses JSON document wrapping parse errors as `Fail`.
decodeData :: (FromJSON a) => [ByteString] -> Outcome a
decodeData fragments =
    case eitherDecode $ fromChunks fragments of
         Left errmsg -> Fail $ show ("Invalid JSON: " ++ errmsg, fragments)
         Right value -> OK value

-- | Main wrapper for API calls. Makes a request, then parses response
--   using `decodeData`.
query :: (FromJSON a) => Manager -> Request -> IO (Outcome a)
query mgr req = decode <$> (work `catch` handleError)
    where
        work = withResponse req mgr handleResult
        handleResult = liftM OK . brConsume . responseBody
        handleError  = return . Fail . fmtHttpError
        decode x = x >>= decodeData

-- | Prepares a request for querying specific API method.
mkRequest :: String -> Remote -> IO Request
mkRequest method sys = setQueryString qs <$> parseUrl url
    where
        url = toString (remoteUrl sys) ++ "api/" ++ method
        qs  = [("api_key", Just $ encodeUtf8 $ remoteApiKey sys)]

mkJsonPostRequest :: Value -> String -> Remote -> IO Request
mkJsonPostRequest payload = mkPostRequest [("payload", toStrict $ encode payload)]

-- | Prepares a request for POSTing to specific API method.
mkPostRequest :: [(ByteString, ByteString)] -> String -> Remote -> IO Request
mkPostRequest args method sys = attachArgs <$> baseRequest
    where
        baseRequest = mkRequest method sys
        attachArgs  = urlEncodedBody args

-- | Retrieves the list of entries on the server.
retrieveIndex :: Client -> IO (Outcome [StoredId])
retrieveIndex (mgr, sys) = mkRequest "index" sys >>= query mgr

-- | Shorthand type.
type EntryIndex = Map Int MD5Sig

-- | Retrieves the list of entries on server as `Map Int String`.
queryIndexAsMap :: Client -> IO (Outcome EntryIndex)
queryIndexAsMap c = toMap <$$> retrieveIndex c
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
updateEntry :: Client -> StoredEntry -> IO (Outcome ())
updateEntry (mgr, sys) e = unwrap <$$> (rq >>= query mgr)
    where
        rq = mkJsonPostRequest (encodeStoredEntry e) "update" sys 

-- | Creates an entry, returning new entry's ID on success.
createEntry :: Client -> NewEntry -> IO (Outcome Int)
createEntry (mgr, sys) e = unwrap <$$> (rq >>= query mgr)
    where
        rq = mkJsonPostRequest (encodeNewEntry e) "create" sys

promoteEntry :: Client -> Int -> IO (Outcome ())
promoteEntry (mgr, sys) uid = unwrap <$$> (rq >>= query mgr) 
    where
        rq = mkPostRequest [("id", pack $ show uid)] "promote" sys

updateRedirect :: Client -> EntryRedirect -> IO (Outcome ())
updateRedirect (mgr, sys) e = unwrap <$$> (rq >>= query mgr)
    where
        rq = mkJsonPostRequest  (encodeRedirect e) "redirect" sys
