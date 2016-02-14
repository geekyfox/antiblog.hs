module Antiblog.Model(
        module Antihost.Model
       ,module Antiblog.Model
       ) where

import Data.Maybe(listToMaybe)

import Anticore.Model
import Antihost.Model

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
    
data Page = Page {
      entries  :: [PagedEntryData]
    -- | Link to page itself.
    , own      :: !String
    -- | Link to the previous page (if any).
    , previous :: Maybe String
    -- | Link to the next page (if any).    
    , next     :: Maybe String
    }
