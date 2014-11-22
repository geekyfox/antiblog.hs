
-- | Entrypoint module of `antisync` utility.
module Main(main) where

import Prelude hiding (lookup)

import Control.Arrow(second)
import Control.Concurrent(threadDelay)
import Control.Monad(liftM, liftM2, filterM, join)
import Data.List(intercalate)
import Data.Map(lookup)
import Data.Maybe(isJust, isNothing, fromJust)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    )
import System.IO(IOMode(ReadMode,WriteMode), withBinaryFile)
import System.IO.Strict(hGetContents)
import System.IO.UTF8(hPutStr)
import System.Posix (fileSize, getFileStatus)

import Antisync.ClientCmdLine
import Antisync.ClientConfig
import Api
import ApiClient
import Model(EntryFS, uid, md5sig)
import Parser(parseText)
import Utils

-- | Reads and parses a file.
loadFile :: SystemName -> FilePath -> IO (Processed EntryFS)
loadFile sys fpath =  
    let
        drain handle = hGetContents handle |>> lines |>> parseText sys
        openAndDrain = withBinaryFile fpath ReadMode drain
        retreat      = return $ Fail "File too big"
    in do
        fsize <- liftM fileSize $ getFileStatus fpath
        if fsize < 100000 then openAndDrain else retreat

-- | Shorthand type for the result of reading multiple files.
type DataFS = [(FilePath, Processed EntryFS)]

-- | Reads and parses multiple files.
loadFiles :: SystemName -> [FilePath] -> IO DataFS
loadFiles sys = mapM parseOne
    where
        parseOne name = loadFile sys name |>> (,) name

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

-- | Matches the entry against the index. Meaning of return value is:
--
--       * `OK` : entry can be synced
--
--       * `Skip` : entry is in sync
--
--       * `Fail` : error occured
--
decideStatus :: EntryIndex -> EntryFS -> Processed String
decideStatus srv e
    | isNothing mfid  = OK "New entry"
    | isNothing mssig = Fail "No matching entry on server"
    | ssig == fsig    = Skip "Not modified"
    | otherwise       = OK "Entry modified"
  where mfid  = uid e
        fid   = fromJust mfid
        mssig = lookup fid srv
        ssig  = fromJust mssig
        fsig  = md5sig e

-- | Monad-ish version of `decideStatus`.
decideStatusM :: EntryIndex -> Processed EntryFS -> Processed String
decideStatusM ix pe = pe >>= decideStatus ix

-- | Shorthand type for statuses of multiple files.
type Status = Processed [(FilePath, Processed String)]

-- | Reads file in the directory and decides their respective statuses.
status :: FilePath -> Endpoint -> IO Status
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
  
-- | Inserts an ID to file on disk.
injectId :: Endpoint -> FilePath -> Int -> IO ()
injectId sys fpath id = load >>= save
    where
        prefix  = "## antiblog public " ++ expose (systemName sys) ++
                  " " ++ show id ++ "\n"
        access  = withBinaryFile fpath
        load    = access ReadMode hGetContents
        save c  = access WriteMode (put c)
        put c h = hPutStr h $ prefix ++ c

-- | Synchronizes single file with remote server.
syncOne :: Endpoint -> EntryIndex -> FilePath -> IO (Processed ())
syncOne sys srv fp = loaded >>= liftProc work |>> join
    where
        approve pe = pe >>= (\e -> decideStatus srv e >> return e)
        loaded     = loadFile (systemName sys) fp |>> approve
        create e   = createEntry sys e |>> unfold
                                       >>= liftProc (injectId sys fp)
        work e
          | isJust $ uid e = updateEntry sys e |>> unfold
          | otherwise      = create e
        unfold = fmap (\(AM x) -> x)

-- | Syncronizes a list of files. Reports progress at given
--   `Verbosity`.
sync :: Verbosity -> [FilePath] -> Endpoint -> IO ()
sync v files sys = queryIndexAsMap sys >>= describeM work >>= putStr
    where
        work srv = mapM (workOne srv) files |>> concat
        workOne srv fp = syncOne sys srv fp |>> report fp
        report fp res
          | shouldIgnore v res = ""
          | otherwise = fp ++ " => " ++ describe (const "OK") res ++ "\n"

-- | Continuously runs in foreground and does `sync` every second.
pump :: [FilePath] -> Endpoint -> IO ()
pump files sys =
    do
        sync Verbose files sys
        threadDelay 1000000
        pump files sys

main :: IO ()
main = decideAction >>= exec

exec :: Action -> IO ()
exec a = act $ actionType a
    where
        sysIO = loadOrDie $ actionEndpoint a
        verbose = actionVerbosity a
        files   = actionFiles a
        act DoNothing = putStrLn helpMessage
        act Status =
            sysIO >>= status "." >>= showStatus verbose
        act Sync =
            sysIO >>= sync verbose files
        act Pump =
            sysIO >>= pump files    
