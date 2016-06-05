{-# LANGUAGE CPP #-}

module Antiblog.ConfigMgt where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.IO.Class
import Data.String(IsString, fromString)
import Data.Text(Text)
import Numeric(showHex)
import System.Console.Haskeline hiding (Handler)
import System.Random(StdGen,newStdGen,randomRs)

import Skulk.Deep
import Skulk.Outcome
import Skulk.ToString

import Antiblog.Config

type Handler a = String -> Either String (Outcome a)

strHandler :: (IsString a) => Handler a
strHandler = Right . OK . fromString

intHandler :: Handler Int
intHandler s = case reads s of
    [(n, "")] -> Right (OK n)
    _ -> Left "Should be an integer number"

boolHandler :: Handler Bool
boolHandler "yes" = Right (OK True)
boolHandler "no" = Right (OK False)
boolHandler _ = Left "Should be 'yes' or 'no'"

ask :: Handler a -> String -> OutcomeIO a
ask handler prompt = Deep $ runInputT defaultSettings go
    where
        go = do
            mline <- getInputLine (prompt ++ ": ")
            case mline of
                Nothing -> return (Skip "User abort")
                Just line -> case handler line of
                    Left msg -> liftIO (putStrLn msg) >> go
                    Right result -> return result

askRq :: Handler a -> String -> OutcomeIO a
askRq handler = ask go
    where
        go "" = Right (Fail "This line is mandatory")
        go line = handler line

askDef :: Handler a -> String -> (String, a) -> OutcomeIO a
askDef handler prompt (defStr, defVal) = ask go extPrompt
    where
        extPrompt = prompt ++ " [" ++ defStr ++ "]"
        go "" = Right (OK defVal)
        go line = handler line

askRqStr :: (IsString a) => String -> OutcomeIO a
askRqStr = askRq strHandler

askDefStr :: (IsString a) => String -> String -> OutcomeIO a
askDefStr prompt def = askDef strHandler prompt (def, fromString def)

askDefInt :: String -> Int -> OutcomeIO Int
askDefInt prompt def = askDef intHandler prompt (show def, def)

askDefBool :: String -> Bool -> OutcomeIO Bool
askDefBool prompt def = askDef boolHandler prompt (if def then "yes" else "no", def)

askAlias :: OutcomeIO SystemName
askAlias = askDefStr "Alias" "antiblog"

askUniqueAlias :: (Config a) => [a] -> OutcomeIO SystemName
askUniqueAlias bundle = do
    value <- askAlias
    assertUnique bundle value
    return value

askRemoteConfig :: [Remote] -> OutcomeIO Remote
askRemoteConfig bundle = do
    name <- askUniqueAlias bundle
    Remote name
        <$> askRqStr "Remote URL"
        <*> askRqStr "API key"

genHex :: StdGen -> String
genHex = foldr showHex "" . randomRs (0, 15 :: Int)

genApiKey :: IO String
genApiKey = (take 32 . genHex) <$> newStdGen

askApiKey :: OutcomeIO Text
askApiKey = fromString <$> do
    apiKey <- inject genApiKey
    askDefStr "API key" apiKey

askHttpPort :: [Local] -> OutcomeIO Int
askHttpPort bundle = do
    let suggest = maximum (8080:[ httpPort x + 1 | x <- bundle ])
    askDefInt "HTTP port" suggest

askAuthorDetails :: OutcomeIO (Bool, String, String)
askAuthorDetails = do
    hasAuthor <- askDefBool "Has \"by <name>\" in page header" True
    authorName <- if hasAuthor then askRqStr "Author name" else return ""
    authorHref <- if hasAuthor then askRqStr "Author URL" else return ""
    return (hasAuthor, authorName, authorHref)

askLocalConfig :: [Local] -> OutcomeIO Local
askLocalConfig bundle = do
    name <- askUniqueAlias bundle
    essential <- Local name
        <$> askDefStr "URL suffix" ""
        <*> askApiKey
        <*> askHttpPort bundle
        <*> askDefStr "Database host" "localhost"
        <*> askDefInt "Database port" 5432        
        <*> askDefStr "Database username" "antiblog"
        <*> askDefStr "Database password" "password"
        <*> askDefStr "Database name" (toString name)
    siteTitle <- askDefStr "Antiblog title" "The Antiblog"
    (hasAuthor, authorName, authorLink) <- askAuthorDetails
    essential siteTitle hasAuthor authorName authorLink
        <$> askDefBool "Has \"Powered by The Antiblog\" in page header" True
        <*> askDefBool "Has \"micro\" tag for short posts" True

addConfig :: (Config a) => FilePath -> a -> OutcomeIO ()
addConfig path item = do
    let sys = systemName item
    bundle <- readBundle path
    assertUnique bundle sys
    inject (writeBundle path (item:bundle))
    
assertUnique :: (Config a) => [a] -> SystemName -> OutcomeIO ()
assertUnique bundle sys = wrap $ case findConfig sys bundle of
    Nothing -> OK ()
    Just _ -> Fail $ "Config section already exists: " ++ toString sys

addRemoteConfig :: IO ()
addRemoteConfig = do
    path <- getClientConfigPath
    describeAndPrint (const "Done") $ do
            bundle <- readBundle path
            cfg <- askRemoteConfig bundle
            addConfig path cfg

addLocalConfig :: IO ()
addLocalConfig = do
    path <- getServerConfigPath
    describeAndPrint (const "Done") $ do
        bundle <- readBundle path
        cfg <- askLocalConfig bundle
        addConfig path cfg
