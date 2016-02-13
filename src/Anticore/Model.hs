-- | Data model.

module Anticore.Model where

import Data.List(intercalate)
import Data.List.Split(splitOn)
import Data.Time.Clock

import Anticore.Utils

-- | Wrapper type for entry title.
newtype Title    = Title String deriving Show
-- | Wrapper type for entry summary.
newtype Summary  = Summary String deriving Show
-- | Wrapper type for entry symlink.
newtype Symlink  = Symlink String deriving Show
-- | Wrapper type for entry "meta" symlink.
newtype Metalink = Metalink String deriving Show
-- | Wrapper type for entry body.
newtype Body     = Body String deriving Show
-- | Wrapper type for entry tags.
newtype Tags     = Tags [String] deriving Show

instance TaggedString Summary where
    expose (Summary s) = s
    wrap = Summary

instance TaggedString Title where
    expose (Title s) = s
    wrap = Title
    
instance TaggedString Body where
    expose (Body s) = s
    wrap = Body
    
instance TaggedString Symlink where
    expose (Symlink s) = s
    wrap = Symlink
    
instance TaggedString Metalink where
    expose (Metalink s) = s 
    wrap = Metalink

instance TaggedString Tags where
    expose (Tags s) = unwords s
    wrap = Tags . words

data SeriesRef = SeriesRef {
      seriesName  :: String
    , seriesIndex :: Int
}

instance Show SeriesRef where
    show (SeriesRef s ix) = concat [
        "Entry #", show ix, " in series '", s, "'"
        ]

instance Encodable SeriesRef where
    encode (SeriesRef s ix) = concat [s, " ", show ix]
    decode s = maybe (Fail $ "Error parsing: '" ++ s ++ "'") OK $
        case words s of
             [a, b] -> fmap (SeriesRef a) $ readInt b
             _      -> Nothing        

newtype SeriesRefList = Series [SeriesRef] deriving Show

instance Encodable SeriesRefList where
    encode (Series ss) = intercalate ";" $ map encode ss
    decode = fmap Series . allOK . map decode . split' ";"
        where
            split' _ [] = []
            split' a b  = splitOn a b

-- | Representation of entry in RSS feed.
data RssEntry = Rss {
    ruid   :: Int,
    rtitle :: Title,
    rsum   :: Summary,
    rsig   :: String,
    posted :: UTCTime,
    rlink  :: String
}

data TagUsage = TagUsage {
     tuTag   :: String
    ,tuCount :: Int
}

data Entry a b c d = Entry {
    -- | Body of the entry.
     body :: Body
    -- | Symbolic link (i.e. http://hostname\/entry\/\<symlink\>).
    ,symlink   :: Maybe Symlink
    -- | Meta-symlink (i.e. http://hostname\/meta\/\<symlink\>). 
    ,metalink  :: Maybe Metalink
    -- | List of tags
    ,tags      :: Tags
    -- | Numeric ID, unique within system.
    , uid       :: a
    -- | Short summary.
    , summary   :: b
    -- | Title.
    , title     :: c
    ,extra :: d
    } deriving Show

data TransportExtra = TREX String SeriesRefList

md5sig :: Entry a b c TransportExtra -> String
md5sig e = let TREX x _ = extra e in x

seriesRef :: Entry a b c TransportExtra -> SeriesRefList
seriesRef e = let TREX _ x = extra e in x

type EntryFS = Entry (Maybe Int) (Maybe Summary) (Maybe Title) TransportExtra
