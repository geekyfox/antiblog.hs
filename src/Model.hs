-- | Data model.

module Model where

import Data.Time.Clock
import Utils

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

-- | Skeleton type for a single blog entry in different contexts.
data Entry a b c d = Entry {
    -- | Body of the entry.
      body      :: Body
    -- | Symbolic link (i.e. http://hostname\/entry\/\<symlink\>).
    , symlink   :: Maybe Symlink
    -- | Meta-symlink (i.e. http://hostname\/meta\/\<symlink\>). 
    , metalink  :: Maybe Metalink
    -- | List of tags
    , tags      :: Tags
    -- | Numeric ID, unique within system.
    , uid       :: a
    -- | Short summary.
    , summary   :: b
    -- | Title.
    , title     :: c
    -- | MD5 signature of entry; used to detect whether entry
    --   was modified or not.
    , md5sig    :: d
    } deriving Show

-- | Entry loaded from filesystem by "Parser".
--   `uid`, `summary` and `title` may be present or absent in the
--   file. `md5sig` is always calculated.
type EntryFS = Entry (Maybe Int) (Maybe Summary) (Maybe Title) String

-- | Entry received by API endpoint.
--   `uid` may be present or absent. `summary` and `md5sig` comply
--   to same rules as in `EntryFS`. When `title` is missing in request,
--   it is replaced with empty string (and hence always present).
type EntryRQ a = Entry a (Maybe Summary) Title String

-- | Entry received by API endpoint for create.
--   Doesn't have `uid` yet.
type EntryCR = EntryRQ ()

-- | Entry received by API endpoint for update.
--   Always has `uid`.
type EntryUP = EntryRQ Int

-- | Entry as loaded from database for rendering.
--   `uid` and `title` are always present and loaded. 
--   `summary` is always present. `md5sig` is never fetched
--   for rendering.
type EntryDB = Entry Int Summary Title ()

-- | Type of rendered webpage. Currently only used to render correct
--   hyperlinks on \/meta\/ pages.
data PageKind = 
    -- | \"Normal\" page (i.e. \/entry\/* or \/page\/*)
    Normal
    -- | \"Meta\" page (i.e. \/meta\/*)
  | Meta

-- | Data that is necessary for rendering an entry on a webpage.
data PagedEntry = PagedEntry {
    entry    :: EntryDB
    -- | Whether or not section needs "read more" link.
  , readMore :: Bool
  , pageKind :: PageKind
}

-- | Data that is necessary for rendering an webpage with multiple
--   entries.
data Page = Page {
      entries  :: [PagedEntry]
    -- | Link to page itself.
    , own      :: !String
    -- | Link to the previous page (if any).
    , previous :: Maybe String
    -- | Link to the next page (if any).    
    , next     :: Maybe String
}

-- | Representation of entry in RSS feed.
data RssEntry = Rss {
    ruid   :: Int,
    rtitle :: Title,
    rsum   :: Summary,
    rsig   :: String,
    posted :: UTCTime,
    rlink  :: String
}
