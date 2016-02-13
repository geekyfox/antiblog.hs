
{-# LANGUAGE OverloadedStrings #-}

-- | Entrypoint module of `antiblog` executable.
module Antiblog.Main(main) where

import Control.Exception.Base(throw, PatternMatchFail(PatternMatchFail))
import Control.Monad(liftM,liftM2,when)
import Control.Monad.IO.Class(liftIO)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Lazy as T
import Network.HTTP.Types.Status(forbidden403, notFound404)
import System.Environment(getArgs)
import Web.Scotty hiding (body)

import Anticore.Api
import Anticore.Model
import Anticore.Utils

import Antiblog.Config
import Antiblog.Database
import Antiblog.Layout hiding (baseUrl,tags,title,summary)

getNotFound :: PoolT -> ConfigSRV -> IO T.Text
getNotFound db cfg = augm |>> renderNotFound
    where
        tags = fetchTagCloud db
        augm = liftM (\ts -> comprise cfg ts ()) tags

-- | Provides an entry page at given URL.
getEntry :: PoolT -> ConfigSRV -> String -> IO T.Text
getEntry db cfg href = entry >>= maybe notFound render
    where
        entry    = fetchEntry db href
        tags     = fetchTagCloud db
        notFound = getNotFound db cfg
        render e = tags |>> combine |>> renderEntry
            where
                combine ts = comprise cfg ts e

-- | Provides a list page at given URL.
getPage :: PoolT -> ConfigSRV -> String -> IO T.Text
getPage db cfg href = augm |>> renderPage
    where
        page = fetchPage db href
        tags = fetchTagCloud db
        augm = liftM2 (comprise cfg) tags page
        
-- | Provides an RSS feed.
getFeed :: PoolT -> ConfigSRV -> IO String
getFeed db cfg = liftM render $ fetchFeed db
    where
        render = renderFeed cfg

-- | Provides a random link.
getRandom :: PoolT -> ConfigSRV -> Maybe T.Text -> IO T.Text
getRandom db cfg ref = link |>> urlConcat base |>> T.pack
    where
        base = baseUrl cfg
        link = case ref of
            Just r  -> fetchRandomExcept db r
            Nothing -> fetchRandom db

mparam :: (Parsable a) => T.Text -> ActionM (Maybe a)
mparam key = fmap Just (param key) `rescue` (\_ -> return Nothing)

defparam :: (Parsable a) => a -> T.Text -> ActionM a
defparam def key = param key `rescue` (\_ -> return def)

encparam :: (Encodable a) => a -> T.Text -> ActionM a
encparam def key = do
    mval <- mparam key
    case mval of
         Nothing -> return def
         Just t -> case decode t of
                        OK v -> return v
                        Skip _ -> return def
                        Fail msg -> throw (PatternMatchFail msg)
                        

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
decodeEntryCreate :: ActionM QueryCR
decodeEntryCreate = decodeEntry (return ())

-- | Decodes params for entry update.
decodeEntryUpdate :: ActionM QueryUP
decodeEntryUpdate = decodeEntry (param "id")

instance Parsable SeriesRef where
    parseParam t =
        case words $ T.unpack t of
             [a, b] -> case readInt b of
                            Just x  -> Right (SeriesRef a x)
                            Nothing -> Left "Parse error"
             _ -> Left "Parse error"

-- | Decodes params for entry create/update.
decodeEntry :: ActionM a -> ActionM (EntryQuery a)
decodeEntry muid = do
    uid      <- muid    
    body     <- param "body"
    md5sig   <- param "signature"
    title    <- defparam "" "title"
    symlink  <- mparam "symlink"
    metalink <- mparam "metalink"
    summary  <- mparam "summary"
    tags     <- defparam "" "tags"
    series   <- encparam (Series []) "series"
    return Entry
        {title    = wrap title
        ,body     = wrap body
        ,symlink  = fmap wrap symlink
        ,metalink = fmap wrap metalink
        ,uid      = uid
        ,summary  = fmap wrap summary
        ,tags     = wrap tags
        ,extra    = TREX md5sig series
        }

decodeEntryPromote :: ActionM Int
decodeEntryPromote = param "id"

-- | Main execution loop.
webloop :: PoolT -> ConfigSRV -> IO ()
webloop db sys =
    let
        invoke_ f        = liftIO (f db sys)
        invoke f arg     = liftIO (f db sys arg)
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
        post "/api/promote" $ secure sys $ do
            uid <- decodeEntryPromote
            result <- liftIO (promoteEntry db uid)
            json result
        notFound $ do
            status notFound404
            invoke_ getNotFound >>= html

-- | Entrypoint.
main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (error "Config file location is missing")
    sys <- serverConfig (head args)    
    db <- mkPool (dbConnString sys)
    webloop db sys
