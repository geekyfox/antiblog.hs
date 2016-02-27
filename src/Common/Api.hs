
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Data model of client-server API.

module Common.Api where

import Control.Applicative
import Control.Monad(mzero)
import Data.Aeson
import Data.ByteString(ByteString)
import Data.ByteString.Lazy(fromChunks)

import Common.Model

import Utils.Data.Outcome

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

-- | Entry received by API endpoint for create.
--   Doesn't have `uid` yet.
type QueryCR = NewEntry

-- | Entry received by API endpoint for update.
--   Always has `uid`.
type QueryUP = StoredEntry

data ApiMessage a = AM a

-- | Response type of `create` method.
type ReplyCR = ApiMessage Int

-- | Response type of `update` method.
type ReplyUP = ApiMessage ()

-- | Response type of `promote` method.
type ReplyPR = ApiMessage ()

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

-- | Parses JSON document wrapping parse errors as `Fail`.
decodeData :: (FromJSON a) => [ByteString] -> Outcome a
decodeData fragments =
    case eitherDecode $ fromChunks fragments of
         Left errmsg -> Fail $ show ("Invalid JSON: " ++ errmsg, fragments)
         Right value -> OK value

