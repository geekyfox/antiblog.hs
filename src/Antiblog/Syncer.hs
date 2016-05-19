{-# LANGUAGE CPP #-}

module Antiblog.Syncer where

import Prelude hiding (lookup)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow(second)
import Control.Concurrent(threadDelay)
import Control.Monad(filterM)
import Data.List(intercalate)
import Data.Map(lookup)

import System.IO(IOMode(ReadMode,WriteMode), withBinaryFile, hPutStr)
import System.IO.Strict(hGetContents)
import System.Directory(doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Posix (fileSize, getFileStatus)

import Skulk.Deep
import Skulk.Outcome
import Skulk.ToString

import Antiblog.Config
import Antiblog.Api
import Antisync.Parser(parseText)
import Common.Model hiding (Normal)

-- | Verbosity level.
data Verbosity = 
    -- | Don't print `Skip` messages.
    Normal
    -- | Don't print "entry is not modified messages".
    | Verbose      
    -- | Print all messages.
    | VeryVerbose

-- | Matches an entry against verbosity setting.
shouldIgnore :: Verbosity -> Outcome a -> Bool
shouldIgnore Verbose (Skip "Not modified") = True
shouldIgnore Normal  (Skip _) = True
shouldIgnore _ _ = False

-- | Shorthand type for the result of reading multiple files.
type DataFS = [(FilePath, Outcome File)]

-- | Reads and parses a file.
loadFile :: SystemName -> FilePath -> IO (Outcome File)
loadFile sys fpath =  
    let
        drain handle = (parseText sys . lines) <$> hGetContents handle
        openAndDrain = withBinaryFile fpath ReadMode drain
        retreat = return $ Fail "File too big"
    in do
        fsize <- fileSize <$> getFileStatus fpath
        if fsize < 100000 then openAndDrain else retreat

promote :: [FilePath] -> Client -> IO ()
promote files client = mapM_ workOne files
    where
        workOne path = do
            result <- checkOne path
            putStrLn $ path ++ " => " ++ describe (const "OK") result
        checkOne path = do
            loaded <- loadFile (systemName $ snd client) path
            reduceBAB $ do
                file <- loaded
                case file of
                    (New _) -> Skip "New entries are not promoted"
                    (Redirect _) -> Skip "Redirected entries are not promoted" 
                    (Stored entry) -> OK $ promoteEntry client $ entryId entry

-- | Syncronizes a list of files. Reports progress at given `Verbosity`.
sync :: Verbosity -> [FilePath] -> Client -> IO ()
sync v files c = output >>= putStrLn
    where
        output :: IO String
        output = queryIndexAsMap c >>= (either return work . toEither)
        work :: EntryIndex -> IO String
        work srv = concat <$> mapM (workOne srv) files
        workOne srv fp = report fp <$> syncOne c srv fp
        report fp res
          | shouldIgnore v res = ""
          | otherwise = fp ++ " => " ++ describe (const "OK") res ++ "\n"

-- | Synchronizes single file with remote server.
syncOne :: Client -> EntryIndex -> FilePath -> IO (Outcome ())
syncOne client index path = do
    let sys = systemName $ snd client
    loaded <- loadFile sys path
    reduceBAB $ do
        entry <- loaded
        decideStatus index entry
        return $ case entry of
                New e -> createEntry client e >=>= injectId sys path
                Stored e -> updateEntry client e
                Redirect e -> updateRedirect client e

-- | Matches the entry against the index. Meaning of return value is:
--
--       * `OK` : entry can be synced
--
--       * `Skip` : entry is in sync
--
--       * `Fail` : error occured
--
decideStatus :: EntryIndex -> File -> Outcome String
decideStatus _ (New _) = OK "New entry"
decideStatus srv (Stored x) = decideStatusImpl srv x
decideStatus srv (Redirect x) = decideStatusImpl srv x

decideStatusImpl :: (HasHash a, Identified a) => EntryIndex -> a -> Outcome String
decideStatusImpl srv e = case lookup (entryId e) srv of
    Nothing -> Fail "No matching entry on server"
    Just s -> if s == md5sig e then Skip "Not modified" else OK "Entry modified"

-- | Inserts an ID to file on disk.
injectId :: SystemName -> FilePath -> Int -> IO ()
injectId sys path eid = do
    text <- withBinaryFile path ReadMode hGetContents
    let prefix = "## antiblog public "  ++ toString sys ++ " " ++ show eid ++ "\n"
    withBinaryFile path WriteMode (\h -> hPutStr h $ prefix ++ text)

-- | Reads and parses multiple files.
loadFiles :: SystemName -> [FilePath] -> IO DataFS
loadFiles sys = mapM (\name -> (,) name <$> loadFile sys name)

-- | Lookups files in directory.
directoryScan :: FilePath -> IO [FilePath]
directoryScan root = do
    c <- getDirectoryContents root
    let ac = [ root ++ "/" ++ i | i <- c, head i /= '.' ]
    fs <- filterM doesFileExist ac
    ds <- filterM doesDirectoryExist ac >>= mapM directoryScan
    return $ concat (fs:ds)

-- | Reads and parses all files in a directory (including subdirs).
loadDir :: SystemName -> FilePath -> IO DataFS
loadDir sys dir = directoryScan dir >>= loadFiles sys

-- | Monad-ish version of `decideStatus`.
decideStatusM :: EntryIndex -> Outcome File -> Outcome String
decideStatusM ix pe = pe >>= decideStatus ix

-- | Shorthand type for statuses of multiple files.
type Status = Outcome [(FilePath, Outcome String)]

-- | Reads file in the directory and decides their respective statuses.
status :: FilePath -> Client -> IO Status
status root client = do
    let sys = systemName $ snd client
    localFiles <- loadDir sys root
    let analyze index = map (second (decideStatusM index)) localFiles
    analyze <$$> queryIndexAsMap client
        
-- | Prints status with given verbosity to stdout.
showStatus :: Verbosity -> Status -> IO ()
showStatus v = putStrLn . describe fun
    where
        fun = intercalate "\n" . filter (not . null) . map fmt
        fmt (fp, ps)
            | shouldIgnore v ps = ""
            | otherwise = "[" ++ describe id ps ++ "] " ++ fp
  
-- | Continuously runs in foreground and does `sync` every second.
pump :: [FilePath] -> Client -> IO ()
pump files c =
    do
        sync Verbose files c
        threadDelay 1000000
        pump files c
