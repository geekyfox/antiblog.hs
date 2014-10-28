{-# LANGUAGE OverloadedStrings #-}

-- | Data model of client-server API.

module Api where

import Control.Monad(mzero)
import Data.Aeson
import Data.ByteString(ByteString)
import Data.ByteString.Lazy(fromChunks)

import Utils

-- | Entry ID and MD5 hash, used by `antisync` to detect whether
--   an entry on the disk is different from an entry on webserver.
data EntryHash = EHash {
    huid    :: Int
  , hash    :: String
}

-- | Response type of `create` method.
type ReplyCR = Int

-- | Response type of `update` method.
type ReplyUP = ()

instance FromJSON EntryHash where
    parseJSON (Object v) = do
        uid <- v .: "id"
        sig <- v .: "signature"
        return $ EHash uid sig
    parseJSON _ = mzero

instance ToJSON EntryHash where
    toJSON (EHash uid sig) = object ["id" .= uid, "signature" .= sig]

-- | Parses JSON document wrapping parse errors as `Fail`.
decodeData :: (FromJSON a) => Processed ByteString -> Processed a
decodeData procBytes = do
    bytes <- procBytes
    case eitherDecode $ fromChunks [bytes] of
         Left errmsg -> Fail $ "Invalid JSON: " ++ errmsg
         Right value -> OK value

