{-# LANGUAGE OverloadedStrings #-}

-- | All generation of HTML pages happens here.
--
--   Since Antiblog is designed to run behind nginx with potentially
--   rewritten URLs, quite some effort is taken to \"rewrite them
--   back\".
--
--   This is the reason why `BaseURL`/`HrefFun` is carried all over
--   the place.

module Antiblog.Layout where

import Control.Applicative
import Control.Monad(when, unless)
import Data.String(IsString,fromString)
import Data.Maybe(fromMaybe,listToMaybe,catMaybes,fromJust)
import Data.Text.Lazy(Text)
import Text.Blaze.Html4.Strict hiding (title,map)
import qualified Text.Blaze.Html4.Strict as H
import Text.Blaze.Html4.Strict.Attributes hiding 
    (title
    ,value
    ,summary
    )
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Text
--import Text.Blaze.Internal(preEscapedText)
import Text.RSS
import Network.URI(parseURI)

import Anticore.Config(BaseURL)
import qualified Anticore.Model as M
import Anticore.Data.Tagged

import qualified Antihost.Config as C

data Augmented a = AUG {
     value :: a
    ,baseUrl :: BaseURL
    -- | Whether or not to add RSS link
    ,hasRssLink :: Bool
    -- | Title (for \<title\> and for OpenGraph section)
    ,title :: M.Title
    -- | URL of the page (for OpenGraph section)
    ,ownUrl :: String
    -- | Summary (for OpenGraph section)
    ,summary :: M.Summary
    ,tags :: [M.TagUsage]
    -- | General title of the website
    ,siteTitle :: !C.SiteTitle
    ,hasAuthor :: !Bool
    ,hasPoweredBy :: !Bool
    ,hasMicroTag :: !Bool
}

class Entity a where
    entityHasRss  :: a -> Bool
    entityTitle   :: a -> Maybe M.Title
    entityUrl     :: BaseURL -> a -> String
    entitySummary :: a -> Maybe M.Summary

comprise :: (Entity a) => C.ConfigSRV -> [M.TagUsage] -> a -> Augmented a
comprise cfg tags x = AUG
    {value      = x
    ,baseUrl    = base
    ,hasRssLink = entityHasRss x
    ,title      = wrap $ generalTitle ++ suffix (entityTitle x)
    ,ownUrl     = entityUrl base x
    ,summary    = unformat $ fromMaybe extendedTitle $ entitySummary x
    ,tags       = tags
    ,siteTitle  = C.siteTitle cfg
    ,hasAuthor  = C.hasAuthor cfg
    ,hasPoweredBy = C.hasPoweredBy cfg
    ,hasMicroTag = C.hasMicroTag cfg
    }
    where
        base = C.baseUrl cfg
        generalTitle = expose $ C.siteTitle cfg
        extendedTitle
            | C.hasAuthor cfg = wrap $ generalTitle ++ " by Ivan Appel"
            | otherwise = wrap generalTitle
        suffix = maybe "" (\x -> ": " ++ x) . fmap expose

instance Entity M.Page where
    entityHasRss _ = True
    entityTitle _   = Nothing
    entityUrl b = fromString . urlConcat b . M.own
    entitySummary _ = Nothing
        
        -- wrap "The Antiblog by Ivan Appel"

instance Entity M.SingleEntry where
    entityHasRss _ = False
    entityTitle = Just . wrap . displayTitle . M.unbox
    entityUrl b = permalink b . M.unbox
    entitySummary = Just . M.summary . M.unbox

instance Entity () where
    entityHasRss _ = False
    entityTitle _ = Nothing
    entityUrl b _ = expose b
    entitySummary _ = Nothing

-- | Produces a `HrefFun` that prepends `BaseURL` 
mkref :: Augmented a -> String -> Attribute
mkref w = href . fromString . urlConcat (baseUrl w)

-- | Strips tags from HTML text
stripTags :: String -> String
stripTags []         = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs

-- | Removes HTML formatting from a `TaggedString`
unformat :: (TaggedString a) => a -> a
unformat = wrap . stripTags . expose

-- | Renders OpenGraph section of \<head\>
opengraph :: Augmented a -> Html
opengraph w =
    do
        item "og:type"        "website"
        item "og:title"       (shapeshift $ title w)
        item "og:url"         (fromString $ ownUrl w)
        item "og:description" (shapeshift $ summary w)
    where
        item p v = meta ! property p ! content v
        property = customAttribute "property"

-- | Display title of the entry (either title or #\$id)
displayTitle :: (IsString b) => M.EntryData -> b
displayTitle e = fromString $ fmt $ expose $ M.title e
    where fmt "" = '#' : show (M.uid e)
          fmt st = st

-- | Permanent link of the entry.
permalink :: (IsString b) => BaseURL -> M.EntryData -> b
permalink base entry = fromString $ urlConcat base preferred
    where
        preferred = fromMaybe standard optional
        standard  = "entry/" ++ show (M.uid entry)
        optional  = listToMaybe $ catMaybes priority
        priority  = case M.pageKind entry of
                         M.Normal -> [symlink, metalink]
                         M.Meta   -> [metalink, symlink]
        symlink   = expose <$> M.symlink entry
        metalink  = expose <$> M.metalink entry

-- | Produces /<head/> part of HTML document.
htmlHead :: Augmented a -> Html
htmlHead w = 
    H.head $ do
        mapM_ cssLink
            [ fontref "Crimson Text:400,400italic,700"
            , fontref "Raleway"
            , fontref "Molengo"
            , fontref "Cutive+Mono"
            , mkref w "static/antiblog.css"
            ]
        when (hasRssLink w) $
            link ! mkref w "rss.xml"
                 ! rel "alternate"
                 ! type_ "application/rss+xml"
                 ! A.title "RSS"
        H.title (shapeshift $ title w)
        opengraph w
    where
        cssLink ref =
            link ! ref ! rel "stylesheet" ! type_ "text/css"
        fontref n =
            href $ fromString
                 $ "http://fonts.googleapis.com/css?family=" ++ n

-- | Produces the \"stamp\" block for top-right corner of the page.
layoutStamp :: Augmented a -> Html
layoutStamp w =
    H.div ! class_ "page-header" $ do
        a ! mkref w "" $ shapeshift $ siteTitle w
        when (hasAuthor w) $
            H.div ! class_ "page-subheader" $ do
                "by "
                a ! href "http://www.geekyfox.net" $ "Ivan Appel"
        when (hasPoweredBy w) $
            H.div ! class_ "page-subheader" $ do
                "powered by "
                a ! href "http://antiblog.geekyfox.net" $ "The Antiblog"
        H.div ! class_ "page-subheader" $ do
            hr
            a ! mkref w "meta/about" $ "About"
            " | "
            a ! mkref w "rss.xml" $ "RSS"
            " | "
            a ! mkref w "entry/random" $ "Random"

prettifyTag = map f
    where
        f '_' = ' '
        f x   = x

patchTags :: Augmented a -> (b -> String) -> [b] -> [b]
patchTags w f xs
  | hasMicroTag w = xs
  | otherwise     = [ x | x <- xs, f x /= "micro" ]

layoutTagCloud :: Augmented a -> Html
layoutTagCloud w
      | null ts = return ()
      | otherwise = (H.div ! class_ "tag-cloud") $ mapM_ one ts
    where
        ts = patchTags w M.tuTag $ tags w
        one (M.TagUsage tag count) = 
            H.div ! class_ (fromString $ classify count) $
                H.div ! class_ "colored" $
                    a ! mkref w ("/page/" ++ tag)
                      $ fromString $ prettifyTag tag
        classify x = "color_" ++ show ((x `mod` 6) + 1)

-- | Attaches the common header to content.
layoutCommon :: Augmented a -> Html -> Html
layoutCommon w content =
    html $ do
        htmlHead w
        body $
            H.div ! class_ "toplevel" $ do
                layoutStamp w
                layoutTagCloud w
                content

-- | Produces a barebone block of HTML with an entry.
layoutEntryBarebone :: (M.RenderEntry b) => Augmented a -> b -> Html
layoutEntryBarebone w@AUG{baseUrl = base} ec =
    H.div ! class_ entryClass $ do
        H.div ! class_ "header colored" $ self (displayTitle e)
        H.div ! class_ bodyClass $
            H.div ! class_ "stuff" $ do
                seriesLinksBlock
                preEscapedText (shapeshift $ M.body e)
                when (M.readMore ec) readMoreBlock    
        unless tagless $
            H.div ! class_ "footer" $
                mapM_ taglink ts'
    where
        e = M.unbox ec
        (M.Tags ts)   = M.tags e
        ts'         = patchTags w Prelude.id ts
        tagless     = null ts'
        modulo      = (M.uid e `mod` 6) + 1        
        entryClass  = fromString $ "entry color_" ++ show modulo
        bodyClass   = if tagless then "body tagless" else "body"
        self t      = a ! href (permalink base e) $ t
        readMoreBlock    = do
                        br
                        "[" ; self "read more" ; "]"
        taglink t   = do
                        preEscapedText "&nbsp;"
                        a ! mkref w ("/page/" ++ t)
                          ! class_ "colored"
                          $ fromString $ prettifyTag t
        seriesLinksBlock =
            case M.seriesLinks ec of
                 Nothing -> return ()
                 Just (M.SL first prev next last) -> H.div ! class_ "series-links" $ do
                     a ! mkref w first $ "First in series"
                     " | "
                     sLink "Previous" prev
                     " | "
                     sLink "Next" next
                     " | "
                     a ! mkref w last $ "Last in series"
        sLink :: Html -> Maybe String -> Html
        sLink txt Nothing = txt
        sLink txt (Just v) = a ! mkref w v $ txt

-- | Produces a complete page with single entry.
layoutEntry :: Augmented M.SingleEntry -> Html
layoutEntry w@AUG{value = paged} = layoutCommon w inner
    where
        inner = layoutEntryBarebone w paged

-- | Produces a complete page with multiple entries
layoutPage :: Augmented M.Page -> Html
layoutPage w@AUG{value = page} = layoutCommon w inner
    where
        navi f css txt =
            case f page of
                 Nothing -> return ()
                 Just h  -> H.div ! class_ css $ a ! mkref w h $ txt
        snippet = layoutEntryBarebone w
        inner = do
            mapM_ snippet $ M.entries page
            H.div ! class_ "navi-container" $ do
                navi M.previous "navi previous" "Previous page"
                navi M.next "navi next" "Next page"  

layoutNotFound :: Augmented () -> Html
layoutNotFound w = layoutCommon w inner
    where
        inner =
            H.div ! class_ "entry" $
                H.div ! class_ "body headless" $
                    "Can't find the page you've been looking for"

-- | Concatenates the urls.
urlConcat :: BaseURL -> String -> String
urlConcat base ('/':xs) = expose base ++ xs
urlConcat base xs       = expose base ++ xs

-- | Renders the RSS feed.
renderFeed :: C.ConfigSRV -> [M.RssEntry] -> String
renderFeed w items = showXML $ rssToXML feed
    where
        base = C.baseUrl w
        feed = RSS  (expose $ C.siteTitle w)
                    (fromJust $ parseURI $ expose base)
                    "/dev/brain >> /dev/null"
                    []
                    (Prelude.map convert items)
        convert i =
            convertTitle (expose $ M.rtitle i) ++
            [ Guid False $ M.rsig i
            , PubDate $ M.posted i
            , Link $ fromJust $ parseURI $ urlConcat base $ M.rlink i
            , Description $ expose $ M.rsum i
            ]
        convertTitle "" = []
        convertTitle ti = [Title ti]


renderEntry :: Augmented M.SingleEntry -> Text
renderEntry = renderHtml . layoutEntry

renderPage :: Augmented M.Page -> Text
renderPage = renderHtml . layoutPage

renderNotFound :: Augmented () -> Text
renderNotFound = renderHtml . layoutNotFound

