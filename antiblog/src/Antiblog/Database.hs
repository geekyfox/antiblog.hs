
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Contains database-related logic.
module Antiblog.Database
       ( module Antiblog.Database
       , module Antihost.Database
       )
where

import Control.Applicative((<$>), (<*>))
import Control.Monad(liftM, liftM2)
-- import Data.ByteString.Char8(pack)
import Data.Maybe(fromMaybe,listToMaybe)
--import Data.Pool(createPool, withResource)
import Data.Pool(withResource)
import Data.String(IsString, fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import qualified Data.Text.Lazy as T

import System.Random(randomRIO)

import Anticore.Api
import Anticore.Model
import Anticore.Utils

import Antihost.Database

import qualified Antiblog.Model as M

deriving instance FromField Summary
deriving instance FromField Body
deriving instance FromField Title
deriving instance FromField Symlink
deriving instance FromField Metalink
deriving instance ToField Summary
deriving instance ToField Body
deriving instance ToField Title
deriving instance ToField Symlink
deriving instance ToField Metalink

instance IsString M.PageKind where
    fromString "list"       = M.Normal
    fromString "entry"      = M.Normal
    fromString "list.meta"  = M.Meta
    fromString "entry.meta" = M.Meta
    fromString x            = error $ "Unsupported tag: " ++ x

instance FromField Tags where
    fromField f m = liftM wrap $ fromField f m

instance FromRow (M.PagedEntryData) where
    fromRow = do
        uid      <- field
        kind     <- field
        title    <- field
        content  <- field
        summary  <- field
        more     <- field
        symlink  <- field
        metalink <- field
        tags     <- field
        let entryData = Entry
                { uid      = uid
                , title    = title
                , body     = content
                , symlink  = symlink
                , metalink = metalink
                , summary  = summary
                , tags     = fromMaybe (Tags []) tags
                , extra    = fromString kind
                }
        let extra = more
        return $ M.PED entryData extra

mkHref :: M.Index -> Maybe String -> String
mkHref a b = show (a, b)

-- | Retrieves a page to show at particular URL.
fetchPage :: PoolT -> M.Index -> Maybe String -> IO M.Page
fetchPage p ix mtag = withResource p go
    where
        go c = liftM2 mix (entries c) (extra c)
        entries c = fetchEntries c ix mtag
        extra   c = fetchPreviousNext c ix mtag
        mix as (a,b) = M.Page as (mkHref ix mtag) a b

instance FromRow M.SeriesLinks where
    fromRow = do
        a <- field
        b <- field
        c <- field
        d <- field
        return $ M.SL a b c d

-- | Retrieves an URL to a random entry.
fetchRandom :: PoolT -> IO String
fetchRandom p = withResource p go
    where
        go c = do
            rs <- query_ c "SELECT id FROM entry ORDER BY random() LIMIT 1"
            case listToMaybe rs of
                 Nothing -> return "/"
                 Just (Only n) -> formatEntryLink c n

instance FromRow RssEntry where
    fromRow = Rss <$> field <*> field <*> field 
                  <*> field <*> field <*> field

-- | Retrieves the list of entries in RSS feed.
fetchFeed :: PoolT -> IO [RssEntry]
fetchFeed p = withResource p go
    where
        go c = get c >>= mapM (decorate c)
        get c = query_ c
            "SELECT e.id, e.title, e.teaser, e.md5_signature, \
            \       re.date_posted AS pubtime                 \
            \FROM entry e, rss_entry re                       \
            \WHERE e.id = re.entry_id                         \
            \ORDER BY re.date_posted DESC"
        decorate c (uid, p, q, r, s) = do
            href <- formatEntryLink c uid
            return (Rss uid p q r s href)

instance FromRow EntryHash where
    fromRow = EHash <$> field <*> field

-- | Retrieves the list of database entries.
fetchEntryIndex :: PoolT -> IO [EntryHash]
fetchEntryIndex p = fetchSimple p
    "SELECT id, md5_signature FROM entry"

assignSymlinks :: Connection -> Int -> Maybe Symlink -> Maybe Metalink -> IO ()
assignSymlinks c uid sl ml = clear >> addSym sl >> addMeta ml
    where
        clear = execute c "DELETE FROM symlink WHERE entry_id = ?" (Only uid)
        addSym Nothing = return ()
        addSym (Just x) = do
            execute c "DELETE FROM symlink WHERE link = ? AND kind = 'normal'" (Only x)
            execute c "INSERT INTO symlink(link, kind, entry_id) VALUES(?, 'normal', ?)" (x, uid)
            return ()
        addMeta Nothing = return ()
        addMeta (Just x) = do
            execute c "DELETE FROM symlink WHERE link = ? AND kind = 'meta'" (Only x)
            execute c "INSERT INTO symlink(link, kind, entry_id) VALUES(?, 'meta', ?)" (x, uid)
            return ()

-- | Records optional data (symlinks, tags) of an entry.
recordOptionalData :: Connection -> EntryQuery a -> Int -> IO ()
recordOptionalData conn e uid =
        recordTags >> recordSeries >> recordSymlinks
    where
        (Tags ts)      = tags e
        (Series series) = seriesRef e
        resetTags      = execute conn
            "DELETE FROM entry_tag WHERE entry_id = ?"
            (Only uid)
        insertTag t    = execute conn
            "INSERT INTO entry_tag(entry_id, tag) VALUES (?, ?)"
            (uid, t::String)
        recordTags     = resetTags >> mapM_ insertTag ts
        recordSymlinks = assignSymlinks conn uid (symlink e) (metalink e)
        resetSeries    = execute conn
            "DELETE FROM series_assignment WHERE entry_id = ?"
            (Only uid)
        insertSeries (SeriesRef s ix) = execute conn
            "INSERT INTO series_assignment(entry_id, series, index) \
            \VALUES (?, ?, ?)"
            (uid, s, ix)
        recordSeries   = resetSeries >> mapM_ insertSeries series

-- | Updates an entry.
updateEntry :: PoolT -> QueryUP -> IO ReplyUP
updateEntry p e = withResource p (\conn -> do
    updateEntryImpl conn (uid e) e
    return $ AM ())
    
updateEntryImpl :: Connection -> Int -> EntryQuery a -> IO ()
updateEntryImpl conn uid e = do
    execute conn "UPDATE entry SET title = ? \
                 \               , teaser = ? \
                 \               , body = ?   \
                 \               , read_more = ? \
                 \               , invisible = FALSE \
                 \               , md5_signature = ? \
                 \WHERE id = ?"
                 (title e, ts, body e, rm, md5sig e, uid)
    recordOptionalData conn e uid
  where
      (ts, rm) = case summary e of
                      Just s -> (s, True)
                      Nothing -> let (p, q) = trim $ expose $ body e in (wrap p, q)
      trim s | length s < 600 = (s, False)
             | otherwise = let x = reverse $ take 600 s
                               y = reverse $ skip ' ' x
                               z = reverse $ skip '\n' x
                           in (if length y > length z then y else z, True)
      skip _ [] = []
      skip y (x:xs) | x == y = xs | otherwise = skip y xs

-- select id from (select round(random() * 9000000 + 1000000) id from generate_series(1,1000)) sq where not exists (select 1 from entry e where e.id = sq.id)

-- | Creates a new entry and returns an assigned ID.
createEntry :: PoolT -> QueryCR -> IO ReplyCR
createEntry p e = withResource p (\conn -> do
    [(Only uid)] <- query_ conn
                           "SELECT id FROM (      \
                           \   SELECT ROUND(RANDOM() * 9000000 + 1000000)::INT id \
                           \   FROM generate_series(1,100) \
                           \) sq WHERE NOT EXISTS ( \
                           \   SELECT 1 FROM entry e \
                           \   WHERE e.id = sq.id \
                           \) LIMIT 1"

    [(Only mmx)] <- query_ conn "SELECT MAX(rank) FROM entry"
    rank <- case mmx of
                 Nothing -> return 1
                 Just n -> do
                     rank <- randomRIO (1, n + 1)
                     slideEntryRanks conn rank
                     return rank
    execute conn "INSERT INTO entry \
                 \(id, title, teaser, body, read_more, invisible, rank, md5_signature) \
                 \VALUES (?, '', '', '', FALSE, TRUE, ?, '')"
                 (uid, rank)
    updateEntryImpl conn uid e
    return $ AM uid)

instance FromRow TagUsage where
    fromRow = TagUsage <$> field <*> field
