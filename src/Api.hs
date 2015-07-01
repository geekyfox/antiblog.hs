{-# LANGUAGE OverloadedStrings #-}

-- | Data model of client-server API.

module Api where

import Control.Monad(mzero)
import Data.Aeson
import Data.ByteString(ByteString)
import Data.ByteString.Lazy(fromChunks)

import Model
import Utils

-- | Skeleton type for a single blog entry in different contexts.
type EntryQuery a = Entry a (Maybe Summary) Title TransportExtra

-- | Entry received by API endpoint for create.
--   Doesn't have `uid` yet.
type QueryCR = EntryQuery ()

-- | Entry received by API endpoint for update.
--   Always has `uid`.
type QueryUP = EntryQuery Int

-- | Entry ID and MD5 hash, used by `antisync` to detect whether
--   an entry on the disk is different from an entry on webserver.
data EntryHash = EHash {
    huid    :: Int
  , hash    :: String
}

data ApiMessage a = AM a

-- | Response type of `create` method.
type ReplyCR = ApiMessage Int

-- | Response type of `update` method.
type ReplyUP = ApiMessage ()

-- | Response type of `promote` method.
type ReplyPR = ApiMessage ()

instance FromJSON EntryHash where
    parseJSON (Object v) = do
        uid <- v .: "id"
        sig <- v .: "signature"
        return $ EHash uid sig
    parseJSON _ = mzero

instance ToJSON EntryHash where
    toJSON (EHash uid sig) = object ["id" .= uid, "signature" .= sig]

instance (FromJSON x) => FromJSON (ApiMessage x) where
    parseJSON (Object v) = do
        content <- v .: "content"
        return $ AM content
    parseJSON _ = mzero

instance (ToJSON x) => ToJSON (ApiMessage x) where
    toJSON (AM x) = object ["content" .= toJSON x]

-- | Parses JSON document wrapping parse errors as `Fail`.
decodeData :: (FromJSON a) => [ByteString] -> Processed a
decodeData fragments =
    case eitherDecode $ fromChunks fragments of
         Left errmsg -> Fail $ show ("Invalid JSON: " ++ errmsg
                                    ,fragments
                                    )
         Right value -> OK value

