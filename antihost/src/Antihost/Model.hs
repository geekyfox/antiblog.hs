module Antihost.Model where

import Data.Char

import Anticore.Model

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
