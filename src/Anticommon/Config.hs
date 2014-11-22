
{-# LANGUAGE OverloadedStrings #-}

{- | Common logic and types shared between client-side and
     server-side runtime configurations.
-}
module Anticommon.Config where

import Control.Monad(liftM)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import System.Directory(getHomeDirectory)
import System.FilePath.Posix(combine)
import Utils

-- | Typesafe wrapper around system's base URL.
newtype BaseURL = BaseURL String

instance TaggedString BaseURL where
    expose (BaseURL s) = s
    wrap x
        | last x == '/' = BaseURL x
        | otherwise     = BaseURL $ x ++ "/"

instance FromJSON BaseURL where
    parseJSON = liftM wrap . parseJSON

-- | Loads settings from a file
load :: (FromJSON a) => FilePath -> IOProc a
load fname = L.readFile fname |>> eitherDecode |>> eitherToProc
    where eitherToProc (Left errmsg) = fail $
            "Error parsing '" ++ fname ++ "': " ++ errmsg
          eitherToProc (Right v) = return v

-- | Loads settings from ~\/\</filename/\>
loadHome :: (FromJSON a) => FilePath -> IOProc a
loadHome suffix = do
    prefix <- getHomeDirectory
    load $ combine prefix suffix
