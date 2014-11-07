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
import Text.Blaze.Html4.Strict
import qualified Text.Blaze.Html4.Strict as H
import Text.Blaze.Html4.Strict.Attributes
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Internal(preEscapedText)
import Text.RSS
import Network.URI(parseURI)

import Config
import qualified Model as M
import Utils

-- | Whether or not to add RSS link to page's header
type LinkRSS = Bool

-- | Data for rendering \<head\> section
data Header = Header {
    -- | Whether or not to add RSS link
    headerRSS     :: LinkRSS
    -- | Title (for \<title\> and for OpenGraph section)
   ,headerTitle   :: M.Title
    -- | URL of the page (for OpenGraph section)
   ,headerURL     :: String
    -- | Summary (for OpenGraph section)
   ,headerSummary :: M.Summary
}

-- | Strips tags from HTML text
stripTags :: String -> String
stripTags []         = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs

-- | Removes HTML formatting from a `TaggedString`
unformat :: (TaggedString a) => a -> a
unformat = wrap . stripTags . expose

-- | Header for page with single entry
entryHeader :: BaseURL -> M.PagedEntry -> Header
entryHeader base paged@M.PagedEntry{ M.entry = e } = Header {
     headerRSS     = False
    ,headerTitle   = wrap $ "The Antiblog: " ++ displayTitle e
    ,headerURL     = permalink base paged
    ,headerSummary = unformat $ M.summary e
}

-- | Header for page with multiple entries
pageHeader :: BaseURL -> M.Page -> Header
pageHeader base M.Page{ M.own = ownUrl } = Header {
     headerRSS     = True
    ,headerTitle   = wrap "The Antiblog"
    ,headerURL     = fromString $ urlConcat base ownUrl
    ,headerSummary = wrap "The Antiblog by Ivan Appel"
}

-- | Renders OpenGraph section of \<head\>
opengraph :: Header -> Html
opengraph header =
    do
        item "og:type"        "website"
        item "og:title"       (shapeshift $ headerTitle header)
        item "og:url"         (fromString $ headerURL header)
        item "og:description" (shapeshift $ headerSummary header)
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

-- | Produces \<head/> part of HTML document.
htmlHead :: BaseURL -> Header -> Html
htmlHead base header = 
    H.head $ do
        mapM_ cssLink
            [ fontref "Crimson Text:400,400italic,700"
            , fontref "Raleway"
            , fontref "Molengo"
            , fontref "Cutive+Mono"
            , mkref base "static/antiblog.css"
            ]
        when (headerRSS header) $
            link ! mkref base "rss.xml"
                 ! rel "alternate"
                 ! type_ "application/rss+xml"
                 ! A.title "RSS"
        H.title (shapeshift $ headerTitle header)
        opengraph header
    where
        cssLink ref =
            link ! ref ! rel "stylesheet" ! type_ "text/css"
        fontref n =
            href $ fromString
                 $ "http://fonts.googleapis.com/css?family=" ++ n

-- | Produces the \"stamp\" block for top-right corner of the page.
layoutStamp :: BaseURL -> Html
layoutStamp base =
    H.div ! class_ "page-header" $ do
        a ! mkref base "" $ "The Antiblog"
        H.div ! class_ "page-subheader" $ do
            "by "
            a ! href "http://www.geekyfox.net" $ "Ivan Appel"
            hr
            H.div ! class_ "page-subheader" $ do
                a ! mkref base "meta/about" $ "About"
                " | "
                a ! mkref base "rss.xml" $ "RSS"
                " | "
                a ! mkref base "entry/random" $ "Random"

-- | Attaches the common header to content.
layoutCommon :: BaseURL -> Header -> Html -> Html
layoutCommon base header content =
    html $ do
        htmlHead base header
        body $
            H.div ! class_ "toplevel" $ do
                layoutStamp base
                content

-- | Produces a barebone block of HTML with an entry.
layoutEntryBarebone :: BaseURL -> M.PagedEntry -> Html
layoutEntryBarebone base paged =
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
                        a ! mkref base ("/page/" ++ t ++ "/1")
                          ! class_ "colored"
                          $ fromString t

-- | Produces a complete page with single entry.
layoutEntry :: BaseURL -> M.PagedEntry -> Html
layoutEntry base paged = layoutCommon base header inner
    where
        inner  = layoutEntryBarebone base paged
        header = entryHeader base paged

-- | Produces a complete page with multiple entries
layoutPage :: BaseURL -> M.Page -> Html
layoutPage base page = layoutCommon base header inner
    where
        header = pageHeader base page
        navi f css txt =
            case f page of
                 Nothing -> return ()
                 Just h  -> H.div ! class_ css $ a ! mkref base h $ txt
        inner = do
            mapM_ (layoutEntryBarebone base) $ M.entries page
            H.div ! class_ "navi-container" $ do
                navi M.previous "navi previous" "Previous page"
                navi M.next     "navi next"     "Next page"  

-- | Produces a `HrefFun` that prepends `BaseURL` 
mkref :: BaseURL -> String -> Attribute
mkref base = href . fromString . urlConcat base

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


renderEntry :: BaseURL -> M.PagedEntry -> Text
renderEntry base = renderHtml . layoutEntry base

renderPage :: BaseURL -> M.Page -> Text
renderPage base = renderHtml . layoutPage base
