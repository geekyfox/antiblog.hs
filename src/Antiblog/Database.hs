
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Contains database-related logic.
module Antiblog.Database where

import Control.Applicative((<$>), (<*>))
import Control.Monad(liftM, liftM2)
import Data.ByteString.Char8(pack)
import Data.Maybe(fromMaybe)
import Data.Pool(Pool, createPool, withResource)
import Data.String(IsString, fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import qualified Data.Text.Lazy as T

import Anticore.Api
import Anticore.Model
import Anticore.Utils

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

-- | Shorthand type name for a pool of PostgreSQL connections.
type PoolT = Pool Connection

-- | Shorthand type name for resultsets of void-typed stored
--   procedures.
type DBVoid = [Only ()]

-- | Creates a connection pool with a given connstring.
mkPool :: String -> IO PoolT
mkPool cs = createPool (connectPostgreSQL $ pack cs) close 1 30 10

-- | Utility function for queries without arguments.
fetchSimple :: (FromRow a) => PoolT -> Query -> IO [a]
fetchSimple pool sql = withResource pool retrieve
    where retrieve conn = query_ conn sql

-- | Utility function for queries with arguments.
fetch :: (ToRow a, FromRow b) => PoolT -> a -> Query -> IO [b]
fetch pool args sql = withResource pool retrieve
    where retrieve conn = query conn sql args

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

-- | Retrieves a list of entries to show at particular URL.
fetchEntries :: PoolT -> String -> IO [M.PagedEntryData]
fetchEntries p href = fetch p (Only href)
    "SELECT entry_id, kind, title, content, teaser,\
    \read_more, symlink, metalink, \"tags\" \
    \FROM page_display WHERE href = ? ORDER BY page_index"
        
-- | Retrieves a page to show at particular URL.
fetchPage :: PoolT -> String -> IO M.Page
fetchPage p href = liftM2 mix entries extra
    where
        entries = fetchEntries p href
        extra   = fetch p (Only href)
            "SELECT previous, next FROM previous_next WHERE href = ?"
        mix as ((a,b):_) = M.Page as href a b
        mix as []        = M.Page as href Nothing Nothing

-- | Retrieves a single entry at particular URL or 'Nothing' if
--   no matching entry found.
fetchEntry :: PoolT -> String -> IO (Maybe M.SingleEntry)
fetchEntry p href = fetchEntries p href >>= complement
    where
        complement [] = return Nothing
        complement (e:_) = do
            let ed = M.unbox e
            sls <- fetchSeries p $ uid ed
            return $ Just $ M.SE ed sls

instance FromRow M.SeriesLinks where
    fromRow = do
        a <- field
        b <- field
        c <- field
        d <- field
        return $ M.SL a b c d
    

fetchSeries :: PoolT -> Int -> IO [M.SeriesLinks]
fetchSeries p id = fetch p (Only id)
    "SELECT first_entry_link, prev_entry_link, next_entry_link,\
    \last_entry_link \
    \FROM series_links WHERE entry_id = ?"

-- | Retrieves an URL to a random entry.
fetchRandom :: PoolT -> IO String
fetchRandom p = fetchSimple p "SELECT random_entry()"
                |>> (head . head)

-- | Retrieves an URL to a random entry that is guaranteed to not be
--   same as mentioned one (`Text` argument is supposed to be
--   Http-Referer header).
fetchRandomExcept :: PoolT -> T.Text -> IO String
fetchRandomExcept p ref = fetch p (Only ref) "SELECT random_entry(?)"
                          |>> (head . head)

instance FromRow RssEntry where
    fromRow = Rss <$> field <*> field <*> field 
                  <*> field <*> field <*> field

-- | Retrieves the list of entries in RSS feed.
fetchFeed :: PoolT -> IO [RssEntry]
fetchFeed p = fetchSimple p
    "SELECT e.id, e.title, e.teaser, e.md5_signature, \
    \       re.date_posted AS pubtime,                \
    \       format_entry_link(e.id) AS permalink      \
    \FROM entry e, rss_entry re                       \
    \WHERE e.id = re.entry_id                         \
    \ORDER BY re.date_posted DESC"

instance FromRow EntryHash where
    fromRow = EHash <$> field <*> field

-- | Retrieves the list of database entries.
fetchEntryIndex :: PoolT -> IO [EntryHash]
fetchEntryIndex p = fetchSimple p
    "SELECT id, md5_signature FROM entry"

-- | Records optional data (symlinks, tags) of an entry.
recordOptionalData :: Connection -> EntryQuery a -> Int -> IO DBVoid
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
        recordSymlinks :: IO DBVoid
        recordSymlinks = query conn
            "SELECT assign_symlinks(?, ?, ?)"
            (uid, symlink e, metalink e)
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
    query conn
          "SELECT update_entry(?, ?, ?, ?, ?)"
          (uid e, title e, body e, summary e, md5sig e) :: IO DBVoid
    recordOptionalData conn e (uid e)
    return $ AM ())

-- | Creates a new entry and returns an assigned ID.
createEntry :: PoolT -> QueryCR -> IO ReplyCR
createEntry p e = withResource p (\conn -> do
    rs <- query conn
                "SELECT create_entry(?, ?, ?, ?)"
                (title e, body e, summary e, md5sig e)
    let uid = case rs of
                   [Only x] -> x
                   _ -> error $ "Weird result set: " ++ show rs
    recordOptionalData conn e uid
    return $ AM uid)

promoteEntry :: PoolT -> Int -> IO ReplyPR
promoteEntry p uid = withResource p (\conn -> do
    query conn
          "SELECT promote_entry(?)"
          (Only uid) :: IO DBVoid
    return $ AM ())

instance FromRow TagUsage where
    fromRow = TagUsage <$> field <*> field

-- | Retrieves the list of entries in RSS feed.
fetchTagCloud :: PoolT -> IO [TagUsage]
fetchTagCloud p = fetchSimple p
    "SELECT tag, counter FROM known_tag"
