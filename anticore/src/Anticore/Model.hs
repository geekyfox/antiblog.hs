-- | Data model.

module Anticore.Model where

import Data.Char(isDigit)
import Data.List(intercalate)
import Data.List.Split(splitOn)
import Data.Maybe(listToMaybe)
import Data.Time.Clock

import Anticore.Data.Outcome
import Anticore.Data.Tagged

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

instance Encodable Tags where
    encode (Tags ts) = unwords ts
    decode = OK . Tags . words

data SeriesRef = SeriesRef String Int

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
    ,symlink :: Maybe Symlink
    -- | Meta-symlink (i.e. http://hostname\/meta\/\<symlink\>). 
    ,metalink :: Maybe Metalink
    -- | List of tags
    ,tags :: Tags
    -- | Numeric ID, unique within system.
    ,uid :: a
    -- | Short summary.
    ,summary :: b
    -- | Title.
    ,title :: c
    ,extra :: d
    } deriving Show

data TransportExtra = TREX String SeriesRefList

class Hashed a where
    md5sig :: a -> String

class Identified a where
    entryId :: a -> Maybe Int

instance (Hashed a, Hashed b) => Hashed (Either a b) where
    md5sig (Left x) = md5sig x
    md5sig (Right x) = md5sig x

instance (Identified a, Identified b) => Identified (Either a b) where
    entryId (Left x) = entryId x
    entryId (Right x) = entryId x

instance Identified Int where
    entryId = Just

instance (Identified a) => Identified (Maybe a) where
    entryId x = x >>= entryId

instance Hashed TransportExtra where
    md5sig (TREX s _) = s

instance (Hashed d) => Hashed (Entry a b c d) where
    md5sig = md5sig . extra

instance (Identified a) => Identified (Entry a b c d) where
    entryId = entryId . uid

seriesRef :: Entry a b c TransportExtra -> SeriesRefList
seriesRef e = let TREX _ x = extra e in x

type EntryFS = Entry (Maybe Int) (Maybe Summary) (Maybe Title) TransportExtra

data SeriesLinks = SL {
    linkFirst :: String,
    linkPrev  :: Maybe String,
    linkNext  :: Maybe String,
    linkLast  :: String
    }

data Index = Number Int | Last deriving Show

parseIndex :: String -> Maybe Index
parseIndex "last" = Just Last
parseIndex x | all isDigit x = Just $ Number $ read x
parseIndex _ = Nothing

parseRef :: String -> (Index, Maybe String)
parseRef s = case parseIndex s of
                  Just ix -> (ix, Nothing)
                  Nothing -> (Number 1, Just s)

-- | Skeleton type for a single blog entry in different contexts.
type EntryData = Entry Int Summary Title PageKind

data PagedEntryData = PED EntryData Bool

-- | Type of rendered webpage. Currently only used to render correct
--   hyperlinks on \/meta\/ pages.
data PageKind = 
    -- | \"Normal\" page (i.e. \/entry\/* or \/page\/*)
    Normal
    -- | \"Meta\" page (i.e. \/meta\/*)
  | Meta
  deriving Show

data SingleEntry = SE EntryData [SeriesLinks]

data Page = Page {
      entries  :: [PagedEntryData]
    -- | Link to page itself.
    , own      :: !String
    -- | Link to the previous page (if any).
    , previous :: Maybe String
    -- | Link to the next page (if any).    
    , next     :: Maybe String
    }

pageKind :: EntryData -> PageKind
pageKind = extra

class RenderEntry e where
    unbox       :: e -> EntryData
    readMore    :: e -> Bool
    seriesLinks :: e -> Maybe SeriesLinks

instance RenderEntry SingleEntry where
    unbox      (SE x _) = x
    readMore    _        = False
    seriesLinks (SE _ y) = listToMaybe y

instance RenderEntry PagedEntryData where
    unbox      (PED x _) = x
    readMore    (PED _ y) = y
    seriesLinks _         = Nothing

data EntryRedirect = RED Int String String (Maybe Symlink) (Maybe Metalink)

instance Hashed EntryRedirect where
    md5sig (RED _ _ s _ _) = s
    
instance Identified EntryRedirect where
    entryId (RED n _ _ _ _) = Just n

redirectUrl (RED _ s _ _ _) = s
