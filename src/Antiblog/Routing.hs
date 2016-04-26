
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Antiblog.Routing where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (empty)
#endif
import Control.Monad.IO.Class(liftIO)
import Data.Aeson hiding (json,Number)
import Data.Aeson.Types hiding (Number)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List(isSuffixOf)
import qualified Data.Text.Lazy as T
import Data.String(IsString)
import Network.HTTP.Types.Status(forbidden403, notFound404)
import Web.Scotty hiding (body)

import Skulk.ToString

import Common.Api
import Antiblog.Config
import Common.Database
import Common.Model

import Antiblog.Layout hiding (title,summary)

import Paths_antiblog

comprise :: PoolT -> Local -> a -> IO (RenderData a)
comprise db cfg entity = do
    tags <- fetchTagCloud db
    return (cfg, tags, entity)

getNotFound :: PoolT -> Local -> IO T.Text
getNotFound db cfg = renderNotFound <$> (comprise db cfg ())

-- | Provides an entry page at given URL.
getEntry :: PoolT -> Local -> String -> PageKind -> IO (Either T.Text String)
getEntry db cfg uid pk = do
    result <- fetchEntry db pk uid
    case result of
        Nothing -> Left <$> getNotFound db cfg
        Just (Right redirect) -> return (Right redirect)
        Just (Left entry) -> (Left . renderEntry) <$> comprise db cfg entry

-- | Provides a list page at given URL.
getPage :: PoolT -> Local -> Index -> Maybe String -> IO T.Text
getPage db cfg ix mtag = renderPage <$> augm
    where
        page = fetchPage db ix mtag
        augm = page >>= (comprise db cfg)
        
-- | Provides an RSS feed.
getFeed :: PoolT -> Local -> IO String
getFeed db cfg = renderFeed cfg <$> fetchRssFeed db

-- | Provides a random link.
getRandom :: PoolT -> Local -> IO T.Text
getRandom db cfg = (T.pack . urlConcat (baseUrl cfg) . toString) <$> fetchRandomEntry db

mparam :: (Parsable a) => T.Text -> ActionM (Maybe a)
mparam key = fmap Just (param key) `rescue` (\_ -> return Nothing)

-- | Wrapper that validates that api_key in request matches one in
--   config.
secure :: Local -> ActionM () -> ActionM ()
secure conf next = mparam "api_key" >>= validate
    where
        validate Nothing = refuse "api_key is missing"
        validate (Just k)
            | k == apiKey conf = next
            | otherwise = refuse "api_key is invalid"
        refuse msg = status forbidden403 >> text msg

-- | Decodes params for entry create.
decodeEntryCreate :: ActionM QueryCR
decodeEntryCreate = decodeEntry (\v -> NewId <$> v .: "signature")

-- | Decodes params for entry update.
decodeEntryUpdate :: ActionM QueryUP
decodeEntryUpdate = decodeEntry (\v -> StoredId <$> v .: "id" <*> v .: "signature")

parsePayload :: (Value -> Parser a) -> ActionM a
parsePayload f = do
    payload <- param "payload"
    case eitherDecode payload of
        Left msg -> fail msg
        Right v -> case parseEither f v of
            Left msg -> fail msg
            Right x -> return x

-- | Decodes params for entry create/update.
decodeEntry :: (Object -> Parser a) -> ActionM (Entry a StoredContent StoredExtra)
decodeEntry uidfun = parsePayload go
    where
        go x = do
            let (Object v) = x
            uid <- uidfun v
            body <- v .: "body"
            title <- nothingToEmpty <$> (v .:? "title")
            symlink <- v .:? "symlink"
            metalink <- v .:? "metalink"
            summary <- nothingToEmpty <$> (v .:? "summary")
            tags <- v .: "tags"
            series <- v .: "series"
            let content = StoredContent title body summary tags
            let extra = StoredExtra series symlink metalink 
            return $ Entry uid content extra

decodeEntryPromote :: ActionM Int
decodeEntryPromote = param "id"

decodeEntryRedirect :: ActionM EntryRedirect
decodeEntryRedirect = parsePayload parseJSON

nameToContentType :: IsString a => String -> a
nameToContentType x
    | "css" `isSuffixOf` x = "text/css"
    | otherwise = "text/plain"

routing db sys =
    let
        renderPage ix mtag = liftIO (getPage db sys ix mtag) >>= html
        renderEntry ref pk = liftIO (getEntry db sys ref pk) >>= either html (redirect . T.pack)
        invoke_ f  = liftIO (f db sys)
    in do
        get "/static/:name" $ do
            name <- param "name"
            fsName <- liftIO (getDataFileName ("/static/" ++ name))
            setHeader "Content-Type" (nameToContentType name)
            file fsName
        get "/entry/random" $
            invoke_ getRandom >>= redirect
        get "/entry/:id" $ do
            ref <- param "id"
            renderEntry ref Normal
        get "/page/:id" $ do
            (ix, mtag) <- parseRef <$> param "id"
            renderPage ix mtag
        get "/page/:tag/:id" $ do
            mix <- parseIndex <$> param "id"
            tag <- param "tag"
            case mix of
                 Just ix -> renderPage ix (Just tag)
                 Nothing -> invoke_ getNotFound >>= html
        get "/" $
            renderPage (Number 1) Nothing
        get "/meta/:id" $ do
            ref <- param "id"
            case parseIndex ref of
                 Just ix -> renderPage ix (Just "meta")
                 Nothing -> renderEntry ref Meta
        get "/rss.xml" $ do
            setHeader "Content-Type" "text/xml"            
            (C.pack <$> invoke_ getFeed) >>= raw
        get "/api/index" $ secure sys $ do
            index <- liftIO (fetchEntryIndex db)
            json index
        post "/api/update" $ secure sys $ do
            e <- decodeEntryUpdate
            result <- liftIO (updateEntry db e)
            json result
        post "/api/create" $ secure sys $ do
            e <- decodeEntryCreate
            result <- liftIO (createEntry db e)
            json result
        post "/api/promote" $ secure sys $ do
            uid <- decodeEntryPromote
            result <- liftIO (promoteEntry db uid)
            json $ AM result
        post "/api/redirect" $ secure sys $ do
            e <- decodeEntryRedirect
            result <- liftIO (updateRedirect db e)
            json result
        notFound $ do
            status notFound404
            invoke_ getNotFound >>= html
