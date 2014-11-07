
{-# LANGUAGE OverloadedStrings #-}

-- | Entrypoint module of `antiblog` executable.
module Main(main) where

import Control.Monad(liftM)
import Control.Monad.IO.Class(liftIO)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Lazy as T
import Network.HTTP.Types.Status(forbidden403, notFound404)
import Web.Scotty

import Database
import Config
import Layout
import Model
import Utils

-- | Provides an entry page at given URL.
getEntry :: PoolT -> BaseURL -> String -> IO T.Text
getEntry db baseUrl href = liftM render $ fetchEntry db href
    where
        notFound = T.pack $ "NOT FOUND: " ++ href
        render   = maybe notFound (renderEntry baseUrl)

-- | Provides a list page at given URL.
getPage :: PoolT -> BaseURL -> String -> IO T.Text
getPage db baseUrl = liftM (renderPage baseUrl) . fetchPage db

-- | Provides an RSS feed.
getFeed :: PoolT -> BaseURL -> IO String
getFeed db baseUrl = liftM (renderFeed baseUrl) $ fetchFeed db

-- | Provides a random link.
getRandom :: PoolT -> BaseURL -> Maybe T.Text -> IO T.Text
getRandom db baseUrl ref = link |>> urlConcat baseUrl |>> T.pack
    where link = case ref of
            Just r  -> fetchRandomExcept db r
            Nothing -> fetchRandom db

mparam :: (Parsable a) => T.Text -> ActionM (Maybe a)
mparam key = fmap Just (param key) `rescue` (\_ -> return Nothing)

defparam :: (Parsable a) => a -> T.Text -> ActionM a
defparam def key = param key `rescue` (\_ -> return def)

-- | Wrapper that validates that api_key in request matches one in
--   config.
secure :: ConfigSRV -> ActionM () -> ActionM ()
secure conf next = mparam "api_key" >>= validate
    where
        validate Nothing       = refuse "api_key is missing"
        validate (Just k)
            | k == apiKey conf = next
            | otherwise        = refuse "api_key is invalid"
        refuse msg = status forbidden403 >> text msg

-- | Decodes params for entry create.
decodeEntryCreate :: ActionM EntryCR
decodeEntryCreate = decodeEntry (return ())

-- | Decodes params for entry update.
decodeEntryUpdate :: ActionM EntryUP
decodeEntryUpdate = decodeEntry (param "id")

-- | Decodes params for entry create/update.
decodeEntry :: ActionM a -> ActionM (EntryRQ a)
decodeEntry muid = do
    uid      <- muid    
    body     <- param "body"
    md5sig   <- param "signature"
    title    <- defparam "" "title"
    symlink  <- mparam "symlink"
    metalink <- mparam "metalink"
    summary  <- mparam "summary"
    tags     <- defparam "" "tags"
    return Entry {
        title      = wrap title,
        Model.body = wrap body,
        symlink    = fmap wrap symlink,
        metalink   = fmap wrap metalink,
        uid        = uid,
        md5sig     = md5sig,
        summary    = fmap wrap summary,
        tags       = wrap tags
    }

-- | Main execution loop.
webloop :: PoolT -> ConfigSRV -> IO ()
webloop db sys =
    let
        invoke_ f        = liftIO (f db $ baseUrl sys)
        invoke f arg     = liftIO (f db (baseUrl sys) arg)
        htmlIO' f url    = invoke f url >>= html
        htmlIO f suf sub = htmlIO' f $ suf ++ sub
    in scotty (httpPort sys) $ do
        get "/entry/random" $
            header "Referer" >>= invoke getRandom >>= redirect
        get "/entry/:id" $
            param "id" >>= htmlIO getEntry "/entry/"
        get "/page/:id" $
            param "id" >>= htmlIO getPage "/page/"
        get "/page/:tag/:id" $ do
            tag <- param "tag"
            param "id" >>= htmlIO getPage ("/page/" ++ tag ++ "/")
        get "/" $
            htmlIO' getPage "/"
        get "/meta/:id" $
            param "id" >>= htmlIO getEntry "/meta/"
        get "/rss.xml" $ do
            setHeader "Content-Type" "text/xml"            
            invoke_ getFeed |>> C.pack >>= raw
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
        notFound $ do
            status notFound404
            html "<h1>This is not the page you are looking for</h1>"            

-- | Entrypoint.
main :: IO ()
main = do
    sys <- sysDefault    
    db <- mkPool (dbConnString sys)
    webloop db sys