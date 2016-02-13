module Antiblog.Model where

import Data.Maybe(listToMaybe)

import Anticore.Model

-- | Type of rendered webpage. Currently only used to render correct
--   hyperlinks on \/meta\/ pages.
data PageKind = 
    -- | \"Normal\" page (i.e. \/entry\/* or \/page\/*)
    Normal
    -- | \"Meta\" page (i.e. \/meta\/*)
  | Meta
  deriving Show

-- | Skeleton type for a single blog entry in different contexts.
type EntryData = Entry Int Summary Title PageKind

pageKind :: EntryData -> PageKind
pageKind = extra

data SeriesLinks = SL {
    linkFirst :: Maybe String,
    linkPrev  :: Maybe String,
    linkNext  :: Maybe String,
    linkLast  :: Maybe String
    }

class RenderEntry e where
    unbox       :: e -> EntryData
    readMore    :: e -> Bool
    seriesLinks :: e -> Maybe SeriesLinks
    
data SingleEntry = SE EntryData [SeriesLinks]

instance RenderEntry SingleEntry where
    unbox      (SE x _) = x
    readMore    _        = False
    seriesLinks (SE _ y) = listToMaybe y

data PagedEntryData = PED EntryData Bool

instance RenderEntry PagedEntryData where
    unbox      (PED x _) = x
    readMore    (PED _ y) = y
    seriesLinks _         = Nothing
    
data Page = Page {
      entries  :: [PagedEntryData]
    -- | Link to page itself.
    , own      :: !String
    -- | Link to the previous page (if any).
    , previous :: Maybe String
    -- | Link to the next page (if any).    
    , next     :: Maybe String
    }
