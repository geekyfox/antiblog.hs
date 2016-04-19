module Antiblog.Syncer where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Arrow(second)
import Control.Concurrent(threadDelay)
import Control.Monad(liftM2, filterM)
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
import Antisync.ApiClient
import Antisync.Parser(parseText)
import Common.Api
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

promote :: [FilePath] -> Remote -> IO ()
promote files sys = mapM_ workOne files
    where
        theFile :: FilePath -> IO (Outcome StoredEntry)
        theFile f = loadFile (systemName sys) f >>== onlyEntry
        workOne :: FilePath -> IO ()
        workOne f = theFile f >>>= process >>= render f
        process :: StoredEntry -> IO (Outcome ReplyPR)
        process x = promoteEntry sys $ entryId x
        render :: FilePath -> Outcome a -> IO ()
        render f p = putStrLn $ f ++ " => " ++ describe (const "OK") p
        onlyEntry :: File -> Outcome StoredEntry
        onlyEntry (New _) = Skip "New entries are not promoted"
        onlyEntry (Redirect _) = Skip "Redirected entries are not promoted"
        onlyEntry (Stored x) = OK x


-- | Syncronizes a list of files. Reports progress at given `Verbosity`.
sync :: Verbosity -> [FilePath] -> Remote -> IO ()
sync v files sys = msg >>= putStrLn
    where
        msg :: IO String
        msg = do
            oix <- queryIndexAsMap sys
            case toEither oix of
                Left msg -> return msg
                Right ix -> work ix
        work :: EntryIndex -> IO String
        work srv = concat <$> mapM (workOne srv) files
        workOne :: EntryIndex -> FilePath -> IO String
        workOne srv fp = report fp <$> syncOne sys srv fp
        report :: FilePath -> Outcome a -> String
        report fp res
          | shouldIgnore v res = ""
          | otherwise = fp ++ " => " ++ describe (const "OK") res ++ "\n"

-- | Synchronizes single file with remote server.
syncOne :: Remote -> EntryIndex -> FilePath -> IO (Outcome ())
syncOne sys srv fp = loaded >>== approve >>>= submit
    where
        loaded :: IO (Outcome File)
        loaded = loadFile (systemName sys) fp
        approve :: File -> Outcome File
        approve e = decideStatus srv e >> return e
        save :: Int -> IO ()
        save = injectId sys fp
        submit :: File -> IO (Outcome ())
        submit (New e) = (unfold <$$> createEntry sys e) >=>= save
        submit (Stored e) = unfold <$$> updateEntry sys e
        submit (Redirect e) = unfold <$$> updateRedirect sys e
        unfold (AM x) = x

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
injectId :: Remote -> FilePath -> Int -> IO ()
injectId sys fpath id = load >>= save
    where
        prefix = "## antiblog public " ++ toString (systemName sys) ++
                  " " ++ show id ++ "\n"
        access = withBinaryFile fpath
        load = access ReadMode hGetContents
        save c = access WriteMode (put c)
        put c h = hPutStr h $ prefix ++ c

-- | Reads and parses multiple files.
loadFiles :: SystemName -> [FilePath] -> IO DataFS
loadFiles sys = mapM parseOne
    where
        parseOne name = (,) name <$> loadFile sys name

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
status :: FilePath -> Remote -> IO Status
status root sys = liftM2 match local remote
    where
        local = loadDir (systemName sys) root
        remote = queryIndexAsMap sys
        match local pix = do
            ix <- pix
            let decide = decideStatusM ix
            return $ map (second decide) local
        
-- | Prints status with given verbosity to stdout.
showStatus :: Verbosity -> Status -> IO ()
showStatus v = putStrLn . describe fun
    where
        fun = intercalate "\n" . filter (not . null) . map fmt
        fmt (fp, ps)
            | shouldIgnore v ps = ""
            | otherwise = "[" ++ describe id ps ++ "] " ++ fp
  
-- | Continuously runs in foreground and does `sync` every second.
pump :: [FilePath] -> Remote -> IO ()
pump files sys =
    do
        sync Verbose files sys
        threadDelay 1000000
        pump files sys
