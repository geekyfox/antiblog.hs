{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data model.

module Common.Model where

import Data.Char(isDigit)
import Data.Time.Clock

import Utils.Data.Tagged

class Identified a where entryId :: a -> Int
instance Identified Int where entryId = id

-- | Wrapper type for entry title.
newtype Title = Title String deriving (Show, TaggedString)
class HasTitle a where title :: a -> Title

-- | Wrapper type for entry body.
newtype Body = Body String deriving (Show, TaggedString)
class HasBody a where body :: a -> Body

-- | Wrapper type for entry summary.
newtype Summary  = Summary String deriving (Show, TaggedString)
class HasSummary a where summary :: a -> Summary

-- | Wrapper type for entry symlink.
newtype Symlink  = Symlink String deriving (Show, TaggedString)
class HasSymlink a where symlink :: a -> Maybe Symlink

-- | Wrapper type for entry "meta" symlink.
newtype Metalink = Metalink String deriving (Show, TaggedString)
class HasMetalink a where metalink :: a -> Maybe Metalink

-- | Wrapper type for entry tags.
newtype Tag = Tag String deriving (Show, TaggedString)
class HasTags a where tags :: a -> [Tag]

-- | Wrapper type for entry hash.
newtype MD5Sig = MD5Sig String deriving (Show, TaggedString, Eq)
class HasHash a where md5sig :: a -> MD5Sig

newtype Permalink = Permalink String deriving (Show, TaggedString)
class HasPermalink a where permalink :: a -> Permalink
instance HasPermalink Permalink where permalink = id

class HasTimestamp a where timestamp :: a -> UTCTime
instance HasTimestamp UTCTime where timestamp = id

data SeriesRef = SeriesRef String Int deriving Show
class HasSeriesRef a where seriesRef :: a -> [SeriesRef]

data PageKind = Normal | Meta deriving Show
class HasPageKind a where pageKind :: a -> PageKind

data Entry a b c = Entry { uid :: a, content :: b, extra :: c } deriving Show
instance (HasHash a) => HasHash (Entry a b c) where md5sig = md5sig . uid
instance (Identified a) => Identified (Entry a b c) where entryId = entryId . uid
instance (HasTitle b) => HasTitle (Entry a b c) where title = title . content
instance (HasSummary b) => HasSummary (Entry a b c) where summary = summary . content
instance (HasBody b) => HasBody (Entry a b c) where body = body . content
instance (HasPermalink b) => HasPermalink (Entry a b c) where permalink = permalink . content
instance (HasTags b) => HasTags (Entry a b c) where tags = tags . content
instance (HasSeriesRef c) => HasSeriesRef (Entry a b c) where seriesRef = seriesRef . extra
instance (HasSymlink c) => HasSymlink (Entry a b c) where symlink = symlink . extra
instance (HasMetalink c) => HasMetalink (Entry a b c) where metalink = metalink . extra
instance (HasSeriesLinks c) => HasSeriesLinks (Entry a b c) where seriesLinks = seriesLinks . extra
instance (HasTimestamp c) => HasTimestamp (Entry a b c) where timestamp = timestamp . extra
instance (HasPageKind c) => HasPageKind (Entry a b c) where pageKind = pageKind . extra

newtype NewId = NewId MD5Sig
instance HasHash NewId where md5sig (NewId x) = x

data StoredId = StoredId Int MD5Sig
instance Identified StoredId where entryId (StoredId x _) = x
instance HasHash StoredId where md5sig (StoredId _ y) = y

data StoredContent = StoredContent Title Body Summary [Tag]
instance HasTitle StoredContent where title (StoredContent x _ _ _) = x
instance HasBody StoredContent where body (StoredContent _ y _ _) = y
instance HasSummary StoredContent where summary (StoredContent _ _ z _) = z
instance HasTags StoredContent where tags (StoredContent _ _ _ w) = w

data StoredExtra = StoredExtra [SeriesRef] (Maybe Symlink) (Maybe Metalink)
instance HasSeriesRef StoredExtra where seriesRef (StoredExtra a _ _) = a
instance HasSymlink StoredExtra where symlink (StoredExtra _ b _) = b
instance HasMetalink StoredExtra where metalink (StoredExtra _ _ c) = c

data RssContent = RssContent Title Summary Permalink
instance HasTitle RssContent where title (RssContent a _ _) = a
instance HasSummary RssContent where summary (RssContent _ b _) = b
instance HasPermalink RssContent where permalink (RssContent  _ _ c) = c

type NewEntry = Entry NewId StoredContent StoredExtra
type StoredEntry = Entry StoredId StoredContent StoredExtra
-- | Representation of entry in RSS feed.
type RssEntry = Entry StoredId RssContent UTCTime

data File = New NewEntry | Stored StoredEntry | Redirect EntryRedirect

data TagUsage = TagUsage String Int

data SeriesLinks = SL {
    linkFirst :: Permalink,
    linkPrev  :: Maybe Permalink,
    linkNext  :: Maybe Permalink,
    linkLast  :: Permalink
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


class HasSeriesLinks a where
    seriesLinks :: a -> [SeriesLinks]

readMore :: (HasBody a, HasSummary a) => a -> Bool
readMore x = (expose $ body x) == (expose $ summary x)

data PagedExtra = PagedExtra (Maybe Symlink) (Maybe Metalink) PageKind

instance HasSymlink PagedExtra where symlink (PagedExtra a _ _) = a
instance HasMetalink PagedExtra where metalink (PagedExtra _ b _) = b
instance HasPageKind PagedExtra where pageKind (PagedExtra _ _ c) = c

instance HasSeriesLinks PagedExtra where
    seriesLinks _ = []

type PagedEntry = Entry Int StoredContent PagedExtra

data SingleExtra = SingleExtra (Maybe Symlink) (Maybe Metalink) PageKind [SeriesLinks]
instance HasSymlink SingleExtra where symlink (SingleExtra a _ _ _) = a
instance HasMetalink SingleExtra where metalink (SingleExtra _ b _ _) = b
instance HasPageKind SingleExtra where pageKind (SingleExtra _ _ c _) = c
instance HasSeriesLinks SingleExtra where seriesLinks (SingleExtra _ _ _ d) = d

type SingleEntry = Entry Int StoredContent SingleExtra

data Page = Page {
      entries  :: [PagedEntry]
    -- | Link to page itself.
    , own      :: !String
    -- | Link to the previous page (if any).
    , previous :: Maybe String
    -- | Link to the next page (if any).    
    , next     :: Maybe String
    }

data RedirectExtra = RedirectExtra (Maybe Symlink) (Maybe Metalink)
instance HasSymlink RedirectExtra where symlink (RedirectExtra x _) = x
instance HasMetalink RedirectExtra where metalink (RedirectExtra _ y) = y

type EntryRedirect = Entry StoredId Permalink RedirectExtra
