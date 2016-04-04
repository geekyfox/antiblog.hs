
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.Database (
    PoolT
    ,connect
    ,createEntry
    ,fetchEntry
    ,fetchEntryIndex
    ,fetchPage
    ,fetchRandomEntry
    ,fetchRssFeed
    ,fetchTagCloud
    ,promoteEntry
    ,rotateEntries
    ,updateEntry
    ,updateRedirect
) where

import Control.Applicative
import Control.Monad(when)
import Data.ByteString.Char8(pack)
import Data.Char(isNumber)
import Data.List(sortBy)
import Data.Maybe
import Data.Ord(comparing)
import Data.Pool(Pool, withResource, createPool)
import Database.PostgreSQL.Simple hiding (connect)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Skulk.Deep((<$$>))

import GHC.Int
import System.Random(randomRIO)

import Common.Api
import Common.Model
import Common.Schema(schema)
import Utils.Data.Tagged

import Utils.Concierge(arrange)

-- | Shorthand type name for a pool of PostgreSQL connections.
type PoolT = Pool Connection

-- | Creates a connection pool with a given connstring.
connect :: String -> IO PoolT
connect cs = do
    db <- createPool (connectPostgreSQL $ pack cs) close 1 30 10
    withResource db (`arrange` schema)
    return db

createEntry :: PoolT -> QueryCR -> IO ReplyCR
createEntry p q = AM <$> withResource p (`createEntryImpl` q)

-- | Updates an entry.
updateEntry :: PoolT -> QueryUP -> IO ReplyUP
updateEntry p e = AM <$> withResource p (\c -> updateEntryImpl c (entryId e) e)

-- | Retrieves a single entry at particular URL or 'Nothing' if
--   no matching entry found.
fetchEntry :: PoolT -> PageKind -> String -> IO (Maybe (Either SingleEntry String))
fetchEntry p pk ref = withResource p (\c -> fetchEntryImpl c pk ref)

-- | Retrieves a page to show at particular URL.
fetchPage :: PoolT -> Index -> Maybe String -> IO Page
fetchPage p ix mtag = withResource p (\c -> fetchPageImpl c ix mtag)

-- | Retrieves the list of entries in RSS feed.
fetchRssFeed :: PoolT -> IO [RssEntry]
fetchRssFeed p = withResource p fetchRssFeedImpl

-- | Retrieves the list of database entries.
fetchEntryIndex :: PoolT -> IO [StoredId]
fetchEntryIndex p = withResource p fetchEntryIndexImpl

promoteEntry :: PoolT -> Int -> IO Int64
promoteEntry p uid = withResource p (`maximizeRank` uid)

-- | Retrieves the count of entries with given tag
fetchTagCloud :: PoolT -> IO [TagUsage]
fetchTagCloud p = withResource p fetchTagCloudImpl

-- | Retrieves an URL to a random entry.
fetchRandomEntry :: PoolT -> IO Permalink
fetchRandomEntry p = withResource p fetchRandomEntryImpl

rotateEntries :: PoolT -> IO [String]
rotateEntries p = withResource p rotateEntriesImpl

-- | Updates a redirect to an entry.
updateRedirect :: PoolT -> EntryRedirect -> IO ReplyUP
updateRedirect p e = AM <$> withResource p (`updateRedirectImpl` e)

--
-- Public API ends here.
--

deriving instance FromField Body
deriving instance ToField Body
deriving instance FromField Metalink
deriving instance ToField Metalink
deriving instance FromField Summary
deriving instance ToField Summary
deriving instance FromField Title
deriving instance ToField Title
deriving instance FromField Symlink
deriving instance ToField Symlink
deriving instance FromField Tag
deriving instance ToField Tag
deriving instance FromField MD5Sig
deriving instance ToField MD5Sig
deriving instance ToField Permalink

queryInt_ :: Connection -> Query -> IO [Int]
queryInt_ c q = fromOnly <$$> query_ c q

queryInt :: (ToRow a) => Connection -> a -> Query -> IO [Int]
queryInt c a q = fromOnly <$$> query c q a

maximizeRank :: Connection -> Int -> IO Int64
maximizeRank conn uid = get >>= set
    where
        get = queryInt_ conn "SELECT MAX(rank) FROM entry"
        set [] = return 0
        set (v:_) = execute conn "UPDATE entry SET rank=? WHERE id=?" (v+1, uid)

slideEntryRanks :: Connection -> Int -> IO ()
slideEntryRanks conn threshold = get >>= mapM_ set
    where
        get = queryInt conn (Only threshold)
            "SELECT id FROM entry WHERE rank >= ? ORDER BY rank DESC"
        set uid = execute conn "UPDATE entry SET rank = rank + 1 WHERE id = ?" (Only uid)

slideFeedPosition :: Connection -> IO ()
slideFeedPosition conn = get >>= mapM_ set
    where
        get = queryInt_ conn "SELECT entry_id FROM rss_entry ORDER BY feed_position DESC"
        set uid = execute conn
            "UPDATE rss_entry SET feed_position = feed_position + 1 WHERE entry_id = ?" (Only uid)

formatEntryLinkInContext :: Connection -> PageKind -> Int -> IO Permalink
formatEntryLinkInContext c pk uid = wrap <$> decide <$> get
    where
        get = query c "SELECT link, kind FROM symlink WHERE entry_id = ?" (Only uid)
        pick :: (Eq a) => a -> [(b,a)] -> Maybe b
        pick x xs = listToMaybe [ b | (b,a) <- xs, a == x ]
        decide :: [(String, String)] -> String
        decide xs = case (pk, pick "normal" xs, pick "meta" xs) of
            (Normal, Just n, _) -> "/entry/" ++ n
            (Meta, _, Just m) -> "/meta/" ++ m
            (_, Just n, _) -> "/entry/" ++ n
            (_, _, Just m) -> "/meta/" ++ m
            _ -> "/entry/" ++ show uid

formatEntryLink :: Connection -> Int -> IO Permalink
formatEntryLink c = formatEntryLinkInContext c Normal

rotateEntriesImpl :: Connection -> IO [String]
rotateEntriesImpl conn = do
        rs <- query_ conn
            "SELECT MAX(rank), COUNT(1)::integer entry_count, invisible \
            \FROM entry GROUP BY invisible ORDER BY invisible"    
        sequence $ case rs::[(Int, Int, Bool)] of
            [] ->
                [return "Database is empty, nothing to rotate."]
            [(_, _, True)] ->
                [return "No visible entries, nothing to rotate"]
            [(mx, ct, False)]
                | mx == ct -> [shift mx, shuffle]
                | otherwise -> [shift mx]
            [(mxv, ctv, False), (mxi, cti, True)]
                | (mxv > mxi) && (mxv == (cti + ctv)) -> [shift mxv, shuffle]
                | mxv > mxi -> [shift mxv]
                | otherwise -> [shift mxv, shiftInvisible (mxi + 1)]
            _ ->
                [return $ "Odd resultset: " ++ show rs]
    where
        shift mx = do
            lnk <- shiftImpl True mx
            return $ "Promoted entry " ++ expose lnk
        shiftInvisible mx = do
            lnk <- shiftImpl False mx
            return $ "Promoted invisible entry " ++ expose lnk
        shiftImpl visible mx = do
            [Only uid] <- query conn "SELECT id FROM entry WHERE rank = ?" (Only mx)            
            slideEntryRanks conn 1
            execute conn "UPDATE entry SET rank = 1 WHERE id = ?" (Only uid)
            execute conn "DELETE FROM rss_entry WHERE entry_id = ?" (Only uid)
            when visible $ do
                slideFeedPosition conn
                execute conn "INSERT INTO rss_entry(entry_id, feed_position) VALUES (?, 1)" (Only uid)
                execute_ conn "DELETE FROM rss_entry WHERE feed_position > 10"
                return ()
            formatEntryLink conn uid
        shuffle = do
            execute_ conn
                "UPDATE entry e SET rank = rank * 2 \
                \WHERE MOD(e.rank, 2) = 1 \
                \AND NOT EXISTS                     \
                \(SELECT 1 FROM rss_entry re WHERE e.id = re.entry_id) \
                \AND NOT EXISTS \
                \(SELECT 1 FROM entry ee WHERE ee.rank = e.rank * 2)"
            return "Performed reshuffling"

fetchSeries :: Connection -> Int -> IO [SeriesLinks]
fetchSeries c uid = go c
    where
        go c = get c >>= mapM (load c)
        get :: Connection -> IO [(String, Int)]
        get c = query c
            "SELECT series, index FROM series_assignment \
            \WHERE entry_id = ?" (Only uid)
        load :: Connection -> (String, Int) -> IO SeriesLinks
        load c (s, ix) = do
            xs <- query c
                "SELECT entry_id, index FROM series_assignment \
                \WHERE series = ? ORDER BY index"
                (Only s)
            f <- formatEntryLink c $ fst $ head xs
            p <- fetch c [ i | (i,j) <- reverse xs, j < ix ]
            n <- fetch c [ i | (i,j) <- xs, j > ix ]
            l <- formatEntryLink c $ fst $ last xs
            return (SL f p n l)
        fetch :: Connection -> [Int] -> IO (Maybe Permalink)
        fetch _ [] = return Nothing
        fetch c (i:_) = Just <$> formatEntryLink c i

encodePageHref :: Maybe String -> Index -> String
encodePageHref Nothing (Number 1) = "/"
encodePageHref Nothing (Number n) = "/page/" ++ show n
encodePageHref Nothing Last = "/page/last"
encodePageHref (Just v) (Number 1) = "/page/" ++ v
encodePageHref (Just v) (Number n) = "/page/" ++ v ++ "/" ++ show n
encodePageHref (Just v) Last = "/page/" ++ v ++ "/last"

fetchEntriesCount :: Connection -> Maybe String -> IO Int
fetchEntriesCount conn (Just "micro") = fetchMicroCount conn
fetchEntriesCount conn (Just "meta") = fetchMetaCount conn
fetchEntriesCount conn mtag = (fromOnly . head) <$> get mtag
    where
        get Nothing =
            query_ conn
                "SELECT COUNT(1)::integer FROM entry \
                \WHERE NOT invisible"
        get (Just v) =
            query conn
                "SELECT COUNT(1)::integer FROM entry_tag \
                \WHERE tag = ?" (Only v)                   

fetchPreviousNext :: Connection -> Index -> Maybe String -> IO (Maybe String, Maybe String)
fetchPreviousNext c ix mtag = do
    entryCount <- fetchEntriesCount c mtag
    let pageCount = (entryCount + 4) `div` 5
    let absIx = case ix of { Last -> pageCount ; Number n -> n }
    let prev = if (absIx <= 1) || (absIx > pageCount) then Nothing
               else Just (encodePageHref mtag (Number $ absIx - 1))
    let next = if (absIx <= 0) || (absIx >= pageCount) then Nothing
               else Just (encodePageHref mtag (Number $ absIx + 1))
    return (prev, next)
    
fetchTagCloudImpl :: Connection -> IO [TagUsage]
fetchTagCloudImpl c = do
    naturals <- query_ c "SELECT tag, COUNT(1)::integer FROM entry_tag GROUP BY tag"
    microCount <- fetchMicroCount c
    metaCount <- fetchMetaCount c
    let synthetics = filter ((> 0) . snd) [("micro", microCount), ("meta", metaCount)]
    return $ map (uncurry TagUsage) $ sortBy (flip (comparing snd)) $ synthetics ++ naturals
    
fetchMicroCount :: Connection -> IO Int
fetchMicroCount conn = do
    [Only x] <- query_ conn
        "SELECT COUNT(1)::integer FROM entry \
        \WHERE teaser = body AND NOT invisible"
    return x

fetchMetaCount :: Connection -> IO Int
fetchMetaCount conn = do
    [Only x] <- query_ conn
        "SELECT COUNT(1)::integer FROM symlink s, entry e \
        \WHERE s.entry_id = e.id AND s.kind = 'meta' AND NOT e.invisible"
    return x

fetchEntryIds :: Connection -> Index -> Maybe String -> IO [Only Int]
fetchEntryIds c (Number n) mtag = fetchEntryIdsImpl c n mtag
fetchEntryIds c Last mtag = do
    ct <- fetchEntriesCount c mtag
    let pg = (ct + 4) `div` 5
    fetchEntryIdsImpl c pg mtag

fetchEntryIdsImpl :: Connection -> Int -> Maybe String -> IO [Only Int]
fetchEntryIdsImpl c n (Just "meta") = query c
    "SELECT id FROM entry e, symlink s \
    \WHERE e.id = s.entry_id AND s.kind = 'meta' AND NOT e.invisible \
    \ORDER BY rank LIMIT 5 OFFSET ?" (Only $ n * 5 - 5)
fetchEntryIdsImpl c n (Just "micro") = query c
    "SELECT id FROM entry e \
    \WHERE teaser = body AND NOT invisible \
    \ORDER BY rank LIMIT 5 OFFSET ? " (Only $ n * 5 - 5)
fetchEntryIdsImpl c n (Just t) = query c
    "SELECT id FROM entry e, entry_tag et \
    \WHERE e.id = et.entry_id AND et.tag = ? \
    \ORDER BY rank LIMIT 5 OFFSET ? " (t, n * 5 - 5)
fetchEntryIdsImpl c n Nothing = query c
    "SELECT id FROM entry e \
    \WHERE NOT invisible \
    \ORDER BY rank LIMIT 5 OFFSET ?" (Only $ n * 5 - 5)
 
fetchEntries :: Connection -> Index -> Maybe String -> IO [PagedEntry]
fetchEntries conn ix mtag = fetchEntryIds conn ix mtag >>= retrieve
    where
        retrieve [] = return []
        retrieve ids = do
            rs <- query conn
                "SELECT id, teaser, body, title \
                \,(SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'normal') \
                \,(SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'meta') \
                \FROM entry e WHERE id IN ? ORDER BY rank" (Only $ In $ map fromOnly ids)
            mapM digest rs
        digest (uid, teaser, body, title, dbSymlink, dbMetalink) = do
            let isMeta = isJust dbMetalink
            let isMicro = (expose teaser) == (expose body)
            let text = if isMicro then Left body else Right teaser
            tags <- fetchTagsFast conn uid isMicro isMeta
            let content = RenderContent title text tags
            let symlink  = (\x -> Symlink $ "entry/" ++ x) <$> dbSymlink
            let metalink = (\x -> Metalink $ "meta/" ++ x) <$> dbMetalink
            let pageKind = if mtag == Just "meta" then Meta else Normal
            let extra = PagedExtra symlink metalink pageKind
            return $ Entry uid content extra

fetchEntryImpl :: Connection -> PageKind -> String -> IO (Maybe (Either SingleEntry String))
fetchEntryImpl c pk ref = do
    muid <- case (pk, ref) of
        (Meta, ref) -> resolveMetalink c ref
        (Normal, ref) | all isNumber ref -> return (Just $ read ref)
        (Normal, ref) -> resolveSymlink c ref
    case muid of
        Just uid -> fetchEntryById c pk uid
        Nothing -> return Nothing

resolveSymlink :: Connection -> String -> IO (Maybe Int)
resolveSymlink c ref = do
    rs <- query c "SELECT entry_id FROM symlink WHERE link = ? AND kind = 'normal'" (Only ref)
    return $ listToMaybe $ map fromOnly rs

resolveMetalink :: Connection -> String -> IO (Maybe Int)
resolveMetalink c ref = do
    rs <- query c "SELECT entry_id FROM symlink WHERE link = ? AND kind = 'meta'" (Only ref)
    return $ listToMaybe $ map fromOnly rs

fetchTagsFast :: Connection -> Int -> Bool -> Bool -> IO [Tag]
fetchTagsFast c uid hasMicro hasMeta = decorate <$> go
    where
        go = do
            rs <- query c "SELECT tag FROM entry_tag WHERE entry_id = ?" (Only uid)
            return $ map fromOnly rs
        decorate xs = xs ++ [wrap "micro" | hasMicro] ++ [wrap "meta" | hasMeta]

fetchEntryById :: Connection -> PageKind -> Int -> IO (Maybe (Either SingleEntry String))
fetchEntryById conn pk uid =  do
    rs <- query conn
            "SELECT redirect_url, body, teaser, title \
            \     , (SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'normal') \
            \     , (SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'meta') \
            \FROM entry e WHERE id = ?" (Only uid)
    case rs of
        [] -> return Nothing
        ((Just a, _, _, _, _, _):_) ->
            return $ Just $ Right a
        ((Nothing, bd, ts, ti, sl, ml):_) -> do
            sls <- fetchSeries conn uid
            let readMore = (expose bd) /= ts
            let hasMicro = not readMore
            tags <- fetchTagsFast conn uid hasMicro (isJust ml)
            let content = RenderContent ti (Left bd) tags
            let symlink = liftT (\x -> "entry/" ++ x) <$> (sl :: Maybe String)
            let metalink = liftT (\x -> "meta/" ++ x) <$> (ml :: Maybe String)
            let extra = SingleExtra symlink metalink pk sls
            return $ Just $ Left $ Entry uid content extra

fetchPageImpl :: Connection -> Index -> Maybe String -> IO Page
fetchPageImpl c ix mtag = do
    entries <- fetchEntries c ix mtag
    (prev, next) <- fetchPreviousNext c ix mtag
    let selfRef = encodePageHref mtag ix
    return (Page entries selfRef prev next)

fetchRandomEntryImpl :: Connection -> IO Permalink
fetchRandomEntryImpl c = do
    rs <- query_ c "SELECT id FROM entry WHERE NOT invisible ORDER BY random() LIMIT 1"
    case rs of
        [] -> return $ wrap "/"
        (Only n:_) -> formatEntryLink c n

fetchRssFeedImpl :: Connection -> IO [RssEntry]
fetchRssFeedImpl conn = get >>= mapM decorate
    where
        get = query_ conn
            "SELECT e.id, e.title, e.teaser, e.md5_signature, re.date_posted AS pubtime \
            \FROM entry e, rss_entry re \
            \WHERE e.id = re.entry_id AND NOT e.invisible \
            \ORDER BY re.date_posted DESC"
        decorate (a,b,c,d,e) = do
            href <- formatEntryLink conn a
            let content = RssContent b c href
            let uid = StoredId a d
            return $ Entry uid content e

fetchEntryIndexImpl :: Connection -> IO [StoredId]
fetchEntryIndexImpl c =  wrap <$$> query_ c "SELECT id, md5_signature FROM entry"
    where
        wrap (a,b) = StoredId a b

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

resetOptionalData :: Connection -> Int -> IO ()
resetOptionalData conn uid = do
    execute conn "DELETE FROM entry_tag WHERE entry_id = ?" (Only uid)
    execute conn "DELETE FROM series_assignment WHERE entry_id = ?" (Only uid)
    return ()

-- | Records optional data (symlinks, tags) of an entry.
-- recordOptionalData :: Connection -> Entry a StoredContent [SeriesRef] -> Int -> IO ()
recordOptionalData conn e uid = do
    resetOptionalData conn uid
    let ts = tags e
    let insertTag t = execute conn
            "INSERT INTO entry_tag(entry_id, tag) VALUES (?, ?)"
            (uid, t::Tag)
    mapM_ insertTag ts
    let series = seriesRef e
    let insertSeries (SeriesRef s ix) = execute conn
            "INSERT INTO series_assignment(entry_id, series, index) \
            \VALUES (?, ?, ?)"
            (uid, s, ix)
    mapM_ insertSeries series
    assignSymlinks conn uid (symlink e) (metalink e)
    
-- updateEntryImpl :: (HasHash a) => Connection -> Int -> Entry a StoredContent [SeriesRef] -> IO ()
updateEntryImpl conn uid e = do
    execute conn
        "UPDATE entry SET title = ?, teaser = ?, body = ? \
        \,invisible = FALSE, md5_signature = ?, redirect_url = NULL \
        \WHERE id = ?"
        (title e, ts, body e, md5sig e, uid)
    recordOptionalData conn e uid
  where
      ts = case nonEmpty $ summary e of
                      Just s -> s
                      Nothing -> wrap $ trim $ expose $ body e
      trim s
          | length s < 600 = s
          | otherwise = let
                      x = reverse $ take 600 s
                      y = reverse $ skip ' ' x
                      z = reverse $ skip '\n' x
                in if length z > 100 then z else y
      skip _ [] = []
      skip y (x:xs) | x == y = xs | otherwise = skip y xs

updateRedirectImpl :: Connection -> EntryRedirect -> IO ()
updateRedirectImpl conn r = do
    execute conn
        "UPDATE entry SET title = '', teaser = '', body = '' \
        \, invisible = TRUE, md5_signature = ? \
        \,redirect_url = ? \
        \WHERE id = ?"
        (md5sig r, permalink r, entryId r)
    resetOptionalData conn (entryId r)
    assignSymlinks conn (entryId r) (symlink r) (metalink r)

-- | Creates a new entry and returns an assigned ID.
createEntryImpl :: Connection -> QueryCR -> IO Int
createEntryImpl conn e = do
    [Only uid] <- query_ conn
                           "SELECT id FROM (      \
                           \   SELECT ROUND(RANDOM() * 9000000 + 1000000)::INT id \
                           \   FROM generate_series(1,100) \
                           \) sq WHERE NOT EXISTS ( \
                           \   SELECT 1 FROM entry e \
                           \   WHERE e.id = sq.id \
                           \) LIMIT 1"
    [Only mmx] <- query_ conn "SELECT MAX(rank) FROM entry"
    rank <- case mmx of
                 Nothing -> return 1
                 Just n -> do
                     rank <- randomRIO (1, n + 1)
                     slideEntryRanks conn rank
                     return rank
    execute conn "INSERT INTO entry \
                 \(id, title, teaser, body, invisible, rank, md5_signature, redirect_url) \
                 \VALUES (?, '', '', '', TRUE, ?, '', NULL)"
                 (uid, rank)
    updateEntryImpl conn uid e
    return uid
