{-# LANGUAGE OverloadedStrings #-}

-- | All generation of HTML pages happens here.
--
--   Since Antiblog is designed to run behind nginx with potentially
--   rewritten URLs, quite some effort is taken to \"rewrite them
--   back\".
--
--   This is the reason why `BaseURL`/`HrefFun` is carried all over
--   the place.

module Layout where

import Control.Monad(when, unless)
import Data.String(IsString,fromString)
import Data.Maybe(fromMaybe,listToMaybe,catMaybes,fromJust)
import Data.Text.Lazy(Text)
import Text.Blaze.Html4.Strict hiding (title)
import qualified Text.Blaze.Html4.Strict as H
import Text.Blaze.Html4.Strict.Attributes hiding 
    (title
    ,value
    ,summary
    )
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Internal(preEscapedText)
import Text.RSS
import Network.URI(parseURI)

import Config(BaseURL)
import qualified Model as M
import Utils

data Augmented a = AUG {
     value :: a
    ,baseUrl :: BaseURL
    -- | Whether or not to add RSS link
    ,hasRssLink :: Bool
    -- | Title (for \<title\> and for OpenGraph section)
    ,title   :: M.Title
    -- | URL of the page (for OpenGraph section)
    ,ownUrl     :: String
    -- | Summary (for OpenGraph section)
    ,summary :: M.Summary
    ,tags :: [M.TagUsage]
}

class Entity a where
    entityHasRss  :: a -> Bool
    entityTitle   :: a -> M.Title
    entityUrl     :: BaseURL -> a -> String
    entitySummary :: a -> M.Summary

comprise :: (Entity a) => BaseURL -> [M.TagUsage] -> a -> Augmented a
comprise base tags x = AUG
    {value   = x
    ,baseUrl = base
    ,hasRssLink = entityHasRss x
    ,title   = entityTitle x
    ,ownUrl  = entityUrl base x
    ,summary = unformat $ entitySummary x
    ,tags    = tags
    }

instance Entity M.Page where
    entityHasRss _ = True
    entityTitle _   = wrap "The Antiblog"
    entityUrl b  = fromString . urlConcat b . M.own
    entitySummary _ = wrap "The Antiblog by Ivan Appel"

instance Entity M.PagedEntry where
    entityHasRss _ = False
    entityTitle  = wrap . (++) "The Antiblog: " . displayTitle . M.entry
    entityUrl = permalink
    entitySummary   = M.summary . M.entry
    
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
displayTitle :: (IsString a) => M.EntryDB -> a
displayTitle e = fromString $ fmt $ expose $ M.title e
    where fmt "" = '#' : show (M.uid e)
          fmt st = st

-- | Permanent link of the entry.
permalink :: (IsString a) => BaseURL -> M.PagedEntry -> a
permalink base paged = fromString $ urlConcat base preferred
    where
        preferred = fromMaybe standard optional
        standard  = "entry/" ++ show (M.uid entry)
        entry     = M.entry paged
        optional  = listToMaybe $ catMaybes priority
        priority  = case M.pageKind paged of
                         M.Normal -> [symlink, metalink]
                         M.Meta   -> [metalink, symlink]
        symlink   = M.symlink entry  |>> expose
        metalink  = M.metalink entry |>> expose

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
layoutStamp w@AUG{baseUrl = base} =
    H.div ! class_ "page-header" $ do
        a ! mkref w "" $ "The Antiblog"
        H.div ! class_ "page-subheader" $ do
            "by "
            a ! href "http://www.geekyfox.net" $ "Ivan Appel"
            hr
            H.div ! class_ "page-subheader" $ do
                a ! mkref w "meta/about" $ "About"
                " | "
                a ! mkref w "rss.xml" $ "RSS"
                " | "
                a ! mkref w "entry/random" $ "Random"
                
layoutTagCloud :: Augmented a -> Html
layoutTagCloud w = (H.div ! class_ "tag-cloud") $ mapM_ one $ tags w
    where
        one (M.TagUsage tag count) = 
            H.div ! class_ (fromString $ classify count) $
                H.div ! class_ "colored" $
                    a ! mkref w ("/page/" ++ tag ++ "/1")
                      $ fromString tag
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
layoutEntryBarebone :: Augmented a -> M.PagedEntry -> Html
layoutEntryBarebone w@AUG{baseUrl = base} paged =
    H.div ! class_ entryClass $ do
        H.div ! class_ "header colored" $ self (displayTitle e)
        H.div ! class_ bodyClass $
            H.div ! class_ "stuff" $ do
                preEscapedText (shapeshift $ M.body e)
                when (M.readMore paged) readMore
        unless tagless $
            H.div ! class_ "footer" $
                mapM_ taglink ts    
    
    where
        e           = M.entry paged
        (M.Tags ts) = M.tags e
        tagless     = null ts
        modulo      = (M.uid e `mod` 6) + 1        
        entryClass  = fromString $ "entry color_" ++ show modulo
        bodyClass   = if tagless then "body tagless" else "body"
        self t      = a ! href (permalink base paged) $ t
        readMore    = do "[" ; self "read more" ; "]"
        taglink t   = do
                        preEscapedText "&nbsp;"
                        a ! mkref w ("/page/" ++ t ++ "/1")
                          ! class_ "colored"
                          $ fromString t

-- | Produces a complete page with single entry.
layoutEntry :: Augmented M.PagedEntry -> Html
layoutEntry w@AUG{value = paged} = layoutCommon w inner
    where
        inner  = layoutEntryBarebone w paged

-- | Produces a complete page with multiple entries
layoutPage :: Augmented M.Page -> Html
layoutPage w@AUG{value = page} = layoutCommon w inner
    where
        navi f css txt =
            case f page of
                 Nothing -> return ()
                 Just h  -> H.div ! class_ css $ a ! mkref w h $ txt
        inner = do
            mapM_ (layoutEntryBarebone w) $ M.entries page
            H.div ! class_ "navi-container" $ do
                navi M.previous "navi previous" "Previous page"
                navi M.next     "navi next"     "Next page"  

-- | Concatenates the urls.
urlConcat :: BaseURL -> String -> String
urlConcat base ('/':xs) = expose base ++ xs
urlConcat base xs       = expose base ++ xs

-- | Renders the RSS feed.
renderFeed :: BaseURL -> [M.RssEntry] -> String
renderFeed base items = showXML $ rssToXML feed
    where
        feed = RSS  "The Antiblog"
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


renderEntry :: Augmented M.PagedEntry -> Text
renderEntry = renderHtml . layoutEntry

renderPage :: Augmented M.Page -> Text
renderPage = renderHtml . layoutPage
