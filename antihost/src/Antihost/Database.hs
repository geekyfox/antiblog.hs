
{-# LANGUAGE OverloadedStrings #-}

module Antihost.Database where

import Control.Applicative
import Control.Monad(
        join
       )
import Data.ByteString.Char8(
        pack
       )
import Data.Char(isNumber)
import Data.List(sortBy)
import Data.List.Split(splitOn)       
import Data.Maybe
import Data.Ord(comparing)
import Data.Pool(
        Pool
       ,withResource
       ,createPool
       )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField(
        FromField
       )
       
import GHC.Int

import Anticore.Model
import Antihost.Model

-- | Shorthand type name for a pool of PostgreSQL connections.
type PoolT = Pool Connection

-- | Shorthand type name for resultsets of void-typed stored
--   procedures.
type DBVoid = [Only ()]

type DBIO a = Connection -> IO a

mutate_ :: Connection -> Query -> IO ()
mutate_ c q = do
    query_ c q :: IO DBVoid
    return ()

mutate :: (ToRow a) => Connection -> Query -> a -> IO ()
mutate c q r = do
    query c q r :: IO DBVoid
    return ()

oneRow_ :: (FromRow a) => Connection -> Query -> IO (Maybe a)
oneRow_ c q = listToMaybe <$> query_ c q

oneField_ :: (FromField a) => Connection -> Query -> IO (Maybe a)
oneField_ c q = join <$> fmap fromOnly <$> oneRow_ c q

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

promoteEntry :: PoolT -> Int -> IO Int64
promoteEntry p uid = withResource p go
    where
        go c = get c >>= maybe (return 0) (set c)
        get :: DBIO (Maybe Int)
        get c = oneField_ c "SELECT MAX(rank) FROM entry"
        set c v = do
            execute c "UPDATE entry SET rank=? WHERE id=?" (v+1, uid)

slideEntryRanks :: Connection -> Int -> IO ()
slideEntryRanks c bottomRank = get >>= mapM_ set
    where
        get :: IO [Only Int]
        get = query c "SELECT id FROM entry WHERE rank >= ? ORDER BY rank DESC" (Only bottomRank)
        set (Only uid) = execute c "UPDATE entry SET rank = rank + 1 WHERE id = ?" (Only uid)

slideFeedPosition :: Connection -> IO ()
slideFeedPosition c = get >>= mapM_ set
    where
        get :: IO [Only Int]
        get = query_ c "SELECT entry_id FROM rss_entry ORDER BY feed_position DESC"
        set (Only uid) = execute c "UPDATE rss_entry SET feed_position = feed_position + 1 WHERE entry_id = ?" (Only uid)

formatEntryLink :: Connection -> Int -> IO String
formatEntryLink c uid = decide <$> get
    where
        get = query c "SELECT link, kind FROM symlink WHERE entry_id = ?" (Only uid)
        decide :: [(String, String)] -> String
        decide xs = head $ [ "/entry/" ++ a | (a,b) <- xs, b == "normal" ] ++
                           [ "/meta/" ++ a | (a,b) <- xs, b == "meta" ]   ++
                           [ "/entry/" ++ show uid ]    

rotateEntries :: PoolT -> IO [String]
rotateEntries p = withResource p go
    where
        go c = do
            [(mmx,ct)] <- stats c
            case mmx of
                 Nothing ->
                     return ["Database is empty, nothing to rotate."]
                 Just mx | mx == ct -> do
                     uid <- lookup c mx
                     shift c uid
                     lnk <- formatEntryLink c uid
                     shuffle c
                     return ["Promoted entry " ++ lnk, "Performed reshuffling"]
                 Just mx -> do
                     uid <- lookup c mx
                     shift c uid
                     lnk <- formatEntryLink c uid
                     return ["Promoted entry " ++ lnk]
        --
        stats :: Connection -> IO [(Maybe Int, Int)]
        stats c = query_ c "SELECT MAX(rank), COUNT(1)::integer entry_count FROM entry"
        --
        lookup :: Connection -> Int -> IO Int
        lookup c mx = do
            [Only uid] <- query c "SELECT id FROM entry WHERE rank = ?" (Only mx)
            return uid
        --
        shift c uid = do
            slideEntryRanks c 1
            execute c "UPDATE entry SET rank = 1 WHERE id = ?" (Only uid)
            execute c "DELETE FROM rss_entry WHERE entry_id = ?" (Only uid)
            slideFeedPosition c
            execute c "INSERT INTO rss_entry(entry_id, feed_position) VALUES (?, 1)" (Only uid)
            execute_ c "DELETE FROM rss_entry WHERE feed_position > 10"
        --
        shuffle c = execute_ c
                       "UPDATE entry e SET rank = rank * 2 \
                       \WHERE MOD(e.rank, 2) = 1 \
                       \AND NOT EXISTS                     \
                       \(SELECT 1 FROM rss_entry re WHERE e.id = re.entry_id) \
                       \AND NOT EXISTS \
                       \(SELECT 1 FROM entry ee WHERE ee.rank = e.rank * 2)"


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
        fetch :: Connection -> [Int] -> IO (Maybe String)
        fetch _ [] = return Nothing
        fetch c (i:_) = Just <$> formatEntryLink c i

decodePageHref :: String -> (Index, Maybe String)
decodePageHref href = go $ splitOn "/" href
    where
        go ["", ""] = (Number 1, Nothing)
        go ["", "page", cs] | all isNumber cs = (Number $ read cs, Nothing)
        go ["", "page", "last"] = (Last, Nothing)
        go ["", "page", cs] = (Number 1, Just cs)
        go ["", "page", cs, ds] | all isNumber ds = (Number $ read ds, Just cs)
        go ["", "page", cs, "last"] = (Last, Just cs)
        go tokens = error ("decodePageHref" ++ show tokens)

encodePageHref :: Maybe String -> Int -> String
encodePageHref Nothing 1 = "/"
encodePageHref Nothing n = "/page/" ++ show n
encodePageHref (Just v) 1 = "/page/" ++ v
encodePageHref (Just v) n = "/page/" ++ v ++ "/" ++ show n

fetchEntriesCount :: Connection -> Maybe String -> IO Int
fetchEntriesCount c (Just "micro") = fetchMicroCount c
fetchEntriesCount c (Just "meta") = fetchMetaCount c
fetchEntriesCount c mtag = (fromOnly . head) <$> get mtag
    where
        get Nothing =
            query_ c "SELECT COUNT(1)::integer FROM entry"
        get (Just v) =
            query c "SELECT COUNT(1)::integer FROM entry_tag WHERE tag = ?" (Only v)                   

fetchPreviousNext :: Connection -> Index -> Maybe String -> IO (Maybe String, Maybe String)
fetchPreviousNext c ix mtag = do
    entryCount <- fetchEntriesCount c mtag
    let pageCount = (entryCount + 4) `div` 5
    let absIx = case ix of { Last -> pageCount ; Number n -> n }
    let prev = if (absIx <= 1) || (absIx > pageCount) then Nothing
               else Just (encodePageHref mtag (absIx - 1))
    let next = if (absIx <= 0) || (absIx >= pageCount) then Nothing
               else Just (encodePageHref mtag (absIx + 1))
    return (prev, next)
    
-- | Retrieves the count of entries with given tag
fetchTagCloud :: PoolT -> IO [TagUsage]
fetchTagCloud p = withResource p go
    where
        wrap = map (uncurry TagUsage) . sortBy (flip (comparing snd))
        go c = do
            xs <- query_ c "SELECT tag, COUNT(1)::integer \
                          \FROM entry_tag GROUP BY tag"
            mc <- fetchMicroCount c
            mt <- fetchMetaCount c
            return $ wrap $ concat [
                 xs
                ,if mc == 0 then [] else [("micro", mc)]
                ,if mt == 0 then [] else [("meta", mt)]
                ]
        

fetchMicroCount :: Connection -> IO Int
fetchMicroCount c = fromOnly <$> head <$> get
    where
        get = query_ c "SELECT COUNT(1)::integer FROM entry WHERE NOT read_more"

fetchMetaCount :: Connection -> IO Int
fetchMetaCount c = fromOnly <$> head <$> get
    where
        get = query_ c "SELECT COUNT(1)::integer FROM symlink WHERE kind = 'meta'"

fetchEntryIds :: Connection -> Index -> Maybe String -> IO [Only Int]
fetchEntryIds c (Number n) mtag = fetchEntryIdsImpl c n mtag
fetchEntryIds c Last mtag = do
    ct <- fetchEntriesCount c mtag
    let pg = (ct + 4) `div` 5
    fetchEntryIdsImpl c pg mtag

fetchEntryIdsImpl :: Connection -> Int -> Maybe String -> IO [Only Int]
fetchEntryIdsImpl c n mtag =
    case mtag of
         Just "meta" ->
             query c "SELECT id FROM entry e, symlink s \
                     \WHERE e.id = s.entry_id AND s.kind = 'meta' \
                     \ORDER BY rank \
                     \LIMIT 5 \
                     \OFFSET ? " (Only $ n * 5 - 5)
         Just "micro" ->
             query c "SELECT id FROM entry e \
                     \WHERE NOT read_more \
                     \ORDER BY rank \
                     \LIMIT 5 \
                     \OFFSET ? " (Only $ n * 5 - 5)
         Just t ->
             query c "SELECT id FROM entry e, entry_tag et \
                     \WHERE e.id = et.entry_id \
                     \  AND et.tag = ? \
                     \ORDER BY rank    \
                     \LIMIT 5 \
                     \OFFSET ? " (t, n * 5 - 5)
         Nothing ->
             query c "SELECT id FROM entry e \
                     \ORDER BY rank          \
                     \LIMIT 5                \
                     \OFFSET ?" (Only $ n * 5 - 5)
 
fetchEntries :: Connection -> Index -> Maybe String -> IO [PagedEntryData]
fetchEntries c ix mtag = fetchEntryIds c ix mtag >>= retrieve
    where
        retrieve [] = return []
        retrieve ids = do
            rs <- query c "SELECT id \
                          \     , teaser \
                          \     , title  \
                          \     , (SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'normal') \
                          \     , (SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'meta') \
                          \     , read_more \
                          \FROM entry e WHERE id IN ? ORDER BY rank" (Only $ In $ map fromOnly ids)
            mapM digest rs
        digest (uid,sm,ti,sl,ml,rm) = do
            tags <- fetchTagsFast c uid (not rm) (isJust ml)
            let entry = Entry {
                         body     = Body sm
                        ,symlink  = (\x -> Symlink $ "entry/" ++ x) <$> sl
                        ,metalink = (\x -> Metalink $ "meta/" ++ x) <$> ml
                        ,tags     = Tags tags
                        ,uid      = uid
                        ,summary  = Summary sm
                        ,title    = Title ti
                        ,extra    = if mtag == (Just "meta") then Meta else Normal
                        }
            return $ PED entry rm

-- | Retrieves a single entry at particular URL or 'Nothing' if
--   no matching entry found.
fetchEntry :: PoolT -> String -> PageKind -> IO (Maybe SingleEntry)
fetchEntry p ref mtag = withResource p (go ref mtag)
    where
        go ref Meta c = do
            muid <- resolveMetalink c ref
            case muid of
                 Nothing -> return Nothing
                 Just uid -> fetchEntryById c uid
        go ref Normal c | all isNumber ref = fetchEntryById c (read ref)
        go ref Normal c = do
            muid <- resolveSymlink c ref
            case muid of
                 Nothing -> return Nothing
                 Just uid -> fetchEntryById c uid

resolveSymlink :: Connection -> String -> IO (Maybe Int)
resolveSymlink c ref = do
    rs <- query c "SELECT entry_id FROM symlink WHERE link = ? AND kind = 'normal'" (Only ref)
    return $ listToMaybe $ map fromOnly rs

resolveMetalink :: Connection -> String -> IO (Maybe Int)
resolveMetalink c ref = do
    rs <- query c "SELECT entry_id FROM symlink WHERE link = ? AND kind = 'meta'" (Only ref)
    return $ listToMaybe $ map fromOnly rs

fetchTagsFast :: Connection -> Int -> Bool -> Bool -> IO [String]
fetchTagsFast c uid hasMicro hasMeta = decorate <$> go
    where
        go = do
            rs <- query c "SELECT tag FROM entry_tag WHERE entry_id = ?" (Only uid)
            return $ map fromOnly rs
        decorate xs = xs ++ ["micro" | hasMicro] ++ ["meta" | hasMeta]

fetchEntryById :: Connection -> Int -> IO (Maybe SingleEntry)
fetchEntryById c uid = go c
    where
        go c = do
            rs <- query c "SELECT body \
                          \     , teaser \
                          \     , title  \
                          \     , (SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'normal') \
                          \     , (SELECT link FROM symlink WHERE entry_id = e.id AND kind = 'meta') \
                          \     , read_more \
                          \FROM entry e WHERE id = ?" (Only uid)
            case rs of
                 [] -> return Nothing
                 [(bd,sm,ti,sl,ml,rm)] -> do
                     sls <- fetchSeries c uid
                     tags <- fetchTagsFast c uid (not rm) (isJust ml)
                     let entry = Entry {
                         body     = Body bd
                        ,symlink  = (\x -> Symlink $ "entry/" ++ x) <$> sl
                        ,metalink = (\x -> Metalink $ "meta/" ++ x) <$> ml
                        ,tags     = Tags tags
                        ,uid      = uid
                        ,summary  = Summary sm
                        ,title    = Title ti
                        ,extra    = Normal
                     }
                     return $ Just $ SE entry sls



