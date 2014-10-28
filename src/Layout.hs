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

type HrefFun = String -> Attribute

-- | Display title of the entry (either title or #\$id)
displayTitle :: (IsString a) => M.EntryDB -> a
displayTitle e = fromString $ fmt $ expose $ M.title e
    where fmt "" = '#' : show (M.uid e)
          fmt st = st

-- | Permanent link of the entry.
permalink :: M.PagedEntry -> String
permalink M.PagedEntry{ M.entry = e, M.pageKind = pk } =
    fromMaybe ("entry/" ++ show (M.uid e))
    $ listToMaybe
    $ catMaybes
    $ prio pk
      where
          prio M.Normal = [sym, met]
          prio M.Meta   = [met, sym]
          sym = M.symlink e  |>> expose
          met = M.metalink e |>> expose

-- | Indicates whether or not to include an RSS link in HTML head.
type LinkRSS = Bool

-- | Produces \<head/> part of HTML document.
htmlHead :: HrefFun -> LinkRSS -> M.Title -> Html
htmlHead mkref showRss title = 
    H.head $ do
        mapM_ cssLink
            [ fontref "Crimson Text:400,400italic,700"
            , fontref "Raleway"
            , fontref "Molengo"
            , fontref "Cutive+Mono"
            , mkref "static/antiblog.css"
            ]
        when showRss $
            link ! mkref "rss.xml"
                 ! rel "alternate"
                 ! type_ "application/rss+xml"
                 ! A.title "RSS"
        H.title (shapeshift title)
    where
        cssLink ref =
            link ! ref ! rel "stylesheet" ! type_ "text/css"
        fontref n = href $ fromString $
            "http://fonts.googleapis.com/css?family=" ++ n

-- | Produces the \"stamp\" block for top-right corner of the page.
layoutStamp :: HrefFun -> Html
layoutStamp mkref =
    H.div ! class_ "page-header" $ do
        a ! mkref "" $ "The Antiblog"
        H.div ! class_ "page-subheader" $ do
            "by "
            a ! href "http://www.geekyfox.net" $ "Ivan Appel"
            hr
            H.div ! class_ "page-subheader" $ do
                a ! mkref "meta/about" $ "About" ; " | "
                a ! mkref "rss.xml" $ "RSS" ; " | "
                a ! mkref "entry/random" $ "Random"

-- | Attaches the common header to content.
layoutCommon :: HrefFun -> LinkRSS -> M.Title -> Html -> Html
layoutCommon mkref showRss title content =
    html $ do
        htmlHead mkref showRss title
        body $
            H.div ! class_ "toplevel" $ do
                layoutStamp mkref
                content

-- | Produces a barebone block of HTML with an entry.
layoutEntryBarebone :: HrefFun -> M.PagedEntry -> Html
layoutEntryBarebone mkref paged =
    H.div ! class_ entryClass $ do
        H.div ! class_ "header colored" $ self (displayTitle e)
        H.div ! class_ bodyClass $
            H.div ! class_ "stuff" $ do
                preEscapedText (shapeshift $ M.body e)
                when (M.readMore paged) $ readMore
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
        self t      = a ! mkref (permalink paged) $ t
        readMore    = do "[" ; self "read more" ; "]"
        taglink t   = a ! class_ "colored"
                        ! mkref ("/page/" ++ t ++ "/1")
                        $ fromString t

-- | Produces a complete page with single entry.
layoutEntry :: HrefFun -> M.PagedEntry -> Html
layoutEntry mkref paged = layoutCommon mkref False title inner
    where
        title = wrap $
            "The Antiblog: " ++ displayTitle (M.entry paged)
        inner = layoutEntryBarebone mkref paged

-- | Produces a complete page with multiple entries
layoutPage :: HrefFun -> M.Page -> Html
layoutPage mkref page = layoutCommon mkref True title inner
    where
        title = wrap "The Antiblog"
        inner = do
            mapM_ (layoutEntryBarebone mkref) $ M.entries page
            H.div ! class_ "navi-container" $ do
                case M.previous page of
                     Nothing -> return ()
                     Just h ->
                         H.div ! class_ "navi previous" $
                             a ! mkref h $ "Previous page"
                case M.next page of
                     Nothing -> return ()
                     Just h ->
                         H.div ! class_ "navi next" $
                         a ! mkref h $ "Next page"

-- | Produces a `HrefFun` that prepends `BaseURL` 
mkref :: BaseURL -> HrefFun
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
renderEntry base = renderHtml . layoutEntry (mkref base)

renderPage :: BaseURL -> M.Page -> Text
renderPage base = renderHtml . layoutPage (mkref base)
