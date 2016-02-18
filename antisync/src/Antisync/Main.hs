
-- | Entrypoint module of `antisync` utility.
module Main(main) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Arrow(second)
import Control.Concurrent(threadDelay)
import Control.Monad(liftM, liftM2, filterM)
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

import Anticore.Api
import Anticore.Control.Flip
import Anticore.Data.Outcome
import Anticore.Data.Tagged
import Anticore.Model

import Antisync.ApiClient
import Antisync.CmdLine
import Antisync.Config
import Antisync.Parser(parseText)

-- | Reads and parses a file.
loadFile :: SystemName -> FilePath -> IO (Outcome EntryFS)
loadFile sys fpath =  
    let
        drain handle = parseText sys <$> lines <$> hGetContents handle
        openAndDrain = withBinaryFile fpath ReadMode drain
        retreat      = return $ Fail "File too big"
    in do
        fsize <- liftM fileSize $ getFileStatus fpath
        if fsize < 100000 then openAndDrain else retreat

-- | Shorthand type for the result of reading multiple files.
type DataFS = [(FilePath, Outcome EntryFS)]

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

-- | Matches the entry against the index. Meaning of return value is:
--
--       * `OK` : entry can be synced
--
--       * `Skip` : entry is in sync
--
--       * `Fail` : error occured
--
decideStatus :: EntryIndex -> EntryFS -> Outcome String
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
decideStatusM :: EntryIndex -> Outcome EntryFS -> Outcome String
decideStatusM ix pe = pe >>= decideStatus ix

-- | Shorthand type for statuses of multiple files.
type Status = Outcome [(FilePath, Outcome String)]

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
syncOne :: Endpoint -> EntryIndex -> FilePath -> IO (Outcome ())
syncOne sys srv fp = submit <!!> approved
    where
        loaded, approved :: IO (Outcome EntryFS)
        loaded = loadFile (systemName sys) fp
        approved = promO approve <!!> loaded
        approve :: EntryFS -> Outcome EntryFS
        approve e = decideStatus srv e >> return e
        save :: Int -> IO ()
        save = injectId sys fp
        submit :: EntryFS -> IO (Outcome ())
        submit e
          | isJust $ uid e = unfold <$$> updateEntry sys e
          | otherwise = promI save <!!> unfold <$$> createEntry sys e
        unfold (AM x) = x

-- | Syncronizes a list of files. Reports progress at given
--   `Verbosity`.
sync :: Verbosity -> [FilePath] -> Endpoint -> IO ()
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

-- | Continuously runs in foreground and does `sync` every second.
pump :: [FilePath] -> Endpoint -> IO ()
pump files sys =
    do
        sync Verbose files sys
        threadDelay 1000000
        pump files sys

promote :: [FilePath] -> Endpoint -> IO ()
promote files sys = mapM_ workOne files
    where
        theFile :: FilePath -> IO (Outcome EntryFS)
        theFile = loadFile (systemName sys)
        workOne :: FilePath -> IO ()
        workOne f = (process <!!> theFile f) >>= render f
        process :: EntryFS -> IO (Outcome ReplyPR)
        process x = reduceM (promoteEntry sys <$> extractId x)
        extractId :: EntryFS -> Outcome Int
        extractId = fromMaybe "ID is missing" . uid
        render :: FilePath -> Outcome a -> IO ()
        render f p = putStrLn $ f ++ " => " ++ describe (const "OK") p

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
        act Promote =
            sysIO >>= promote files
