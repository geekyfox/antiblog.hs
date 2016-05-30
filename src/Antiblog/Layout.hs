
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- | All generation of HTML pages happens here.
--
--   Since Antiblog is designed to run behind nginx with potentially
--   rewritten URLs, quite some effort is taken to \"rewrite them
--   back\".
--
--   This is the reason why `BaseURL`/`HrefFun` is carried all over
--   the place.

module Antiblog.Layout where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad(when, unless)
import Data.String(IsString, fromString)
import Data.Maybe(fromMaybe,listToMaybe,catMaybes)
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
import Text.Feed.Types
import Text.Feed.Util
import Text.RSS.Export
import Text.RSS.Syntax
import Text.XML.Light.Output

import Skulk.ToString

import qualified Common.Model as M
import Antiblog.Config

class Entity a where
    entityHasRss :: a -> Bool
    entityTitle :: a -> Maybe M.Title
    entityUrl :: BaseURL -> a -> String
    entityText :: a -> Maybe (Either M.Body M.Summary)

type RenderData a = (Local, [M.TagUsage], a)

summary :: (Entity a) => Local -> a -> M.Summary
summary cfg entity = liftT stripTags $ fromMaybe extendedTitle $ shapeshift <$> entityText entity
    where
        generalTitle :: SiteTitle
        generalTitle = siteTitle cfg
        extendedTitle :: M.Summary
        extendedTitle
            | hasAuthor cfg = fromString $ toString generalTitle ++ " by " ++ toString (author cfg)
            | otherwise = shapeshift generalTitle

title :: (Entity a) => Local -> a -> M.Title
title cfg entity = fromString $ toString (siteTitle cfg) ++ suffix
    where
        suffix = case entityTitle entity of
            Nothing -> ""
            Just s -> ": " ++ toString s

ownUrl :: (Entity a) => Local -> a -> String
ownUrl cfg = entityUrl (baseUrl cfg)

instance Entity M.Page where
    entityHasRss _ = True
    entityTitle _   = Nothing
    entityUrl b = fromString . urlConcat b . M.own
    entityText _ = Nothing
        
instance Entity M.SingleEntry where
    entityHasRss _ = False
    entityTitle = Just . displayTitle
    entityUrl = permalink
    entityText = Just . M.bodyOrSummary

instance Entity () where
    entityHasRss _ = False
    entityTitle _ = Nothing
    entityUrl b _ = toString b
    entityText _ = Nothing

-- | Produces a `HrefFun` that prepends `BaseURL` 
mkref :: Local -> String -> Attribute
mkref cfg = href . fromString . urlConcat (baseUrl cfg)

-- | Strips tags from HTML text
stripTags :: String -> String
stripTags [] = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs

-- | Renders OpenGraph section of \<head\>
opengraph :: Entity a => Local -> a -> Html
opengraph cfg entity =
    do
        item "og:type" "website"
        item "og:title" (shapeshift $ title cfg entity)
        item "og:url" (fromString $ ownUrl cfg entity)
        item "og:description" (shapeshift $ summary cfg entity)
    where
        item p v = meta ! property p ! content v
        property = customAttribute "property"

-- | Display title of the entry (either title or #\$id)
displayTitle :: (M.HasTitle a, M.Identified a) => a -> M.Title
displayTitle e = liftT fmt (M.title e)
    where
        fmt "" = '#' : show (M.entryId e)
        fmt st = st

-- | Permanent link of the entry.
permalink :: (M.HasSymlink c, M.HasMetalink c, M.HasPageKind c, IsString d)
   => BaseURL -> M.Entry Int b c -> d
permalink base entry = fromString $ urlConcat base preferred
    where
        preferred = fromMaybe standard optional
        standard  = "entry/" ++ show (M.uid entry)
        optional  = listToMaybe $ catMaybes priority
        priority  = case M.pageKind entry of
            M.Normal -> [symlink, metalink]
            M.Meta -> [metalink, symlink]
        symlink   = toString <$> M.symlink entry
        metalink  = toString <$> M.metalink entry

-- | Produces /<head/> part of HTML document.
htmlHead :: (Entity a) => Local -> a -> Html
htmlHead cfg entity = 
    H.head $ do
        mapM_ cssLink
            [ fontref "Crimson Text:400,400italic,700"
            , fontref "Raleway"
            , fontref "Molengo"
            , fontref "Cutive+Mono"
            , mkref cfg "static/antiblog.css"
            ]
        when (entityHasRss entity) $
            link ! mkref cfg "rss.xml"
                 ! rel "alternate"
                 ! type_ "application/rss+xml"
                 ! A.title "RSS"
        H.title (shapeshift $ title cfg entity)
        opengraph cfg entity
    where
        cssLink ref =
            link ! ref ! rel "stylesheet" ! type_ "text/css"
        fontref n =
            href $ fromString
                 $ "http://fonts.googleapis.com/css?family=" ++ n

-- | Produces the \"stamp\" block for top-right corner of the page.
layoutStamp :: Local -> Html
layoutStamp cfg =
    H.div ! class_ "page-header" $ do
        a ! mkref cfg "" $ shapeshift $ siteTitle cfg
        when (hasAuthor cfg) $
            H.div ! class_ "page-subheader" $ do
                "by "
                a ! href (shapeshift $ authorHref cfg) $ shapeshift (author cfg)
        when (hasPoweredBy cfg) $
            H.div ! class_ "page-subheader" $ do
                "powered by "
                a ! href "http://github.com/geekyfox/antiblog" $ "The Antiblog"
        H.div ! class_ "page-subheader" $ do
            hr
            a ! mkref cfg "meta/about" $ "About"
            " | "
            a ! mkref cfg "rss.xml" $ "RSS"
            " | "
            a ! mkref cfg "entry/random" $ "Random"

prettifyTag = map f
    where
        f '_' = ' '
        f x   = x

patchTags :: Local -> (b -> String) -> [b] -> [b]
patchTags cfg f xs
  | hasMicroTag cfg = xs
  | otherwise = [ x | x <- xs, f x /= "micro" ]

layoutTagCloud :: Local -> [M.TagUsage] -> Html
layoutTagCloud cfg tags
      | null ts = return ()
      | otherwise = (H.div ! class_ "tag-cloud") $ mapM_ one ts
    where
        ts = patchTags cfg (\(M.TagUsage s _) -> s) tags
        one (M.TagUsage tag count) =
            H.div ! class_ (fromString $ classify count) $
                H.div ! class_ "colored" $
                    a ! mkref cfg ("/page/" ++ tag)
                      $ fromString $ prettifyTag tag
        classify x = "color_" ++ show ((x `mod` 6) + 1)

-- | Attaches the common header to content.
layoutCommon :: (Entity a) => (Local -> a -> Html) -> RenderData a -> Html
layoutCommon innerFunc (cfg, tags, entity) =
    html $ do
        htmlHead cfg entity
        body $
            H.div ! class_ "toplevel" $ do
                layoutStamp cfg
                layoutTagCloud cfg tags
                innerFunc cfg entity

-- | Produces a barebone block of HTML with an entry.
layoutEntryBarebone :: (M.HasSeriesLinks c, M.HasSymlink c, M.HasMetalink c, M.HasPageKind c)
    => Local -> M.Entry Int M.RenderContent c -> Html
layoutEntryBarebone cfg e =
    H.div ! class_ entryClass $ do
        H.div ! class_ "header colored" $ self (shapeshift $ displayTitle e)
        H.div ! class_ bodyClass $
            H.div ! class_ "stuff" $ do
                seriesLinksBlock
                preEscapedText (shapeshift $ M.bodyOrSummary e)
                when (M.readMore e) readMoreBlock    
        unless tagless $
            H.div ! class_ "footer" $
                mapM_ taglink ts'
    where
        ts = map toString $ M.tags e
        ts' = patchTags cfg Prelude.id ts
        tagless = null ts'
        modulo = (M.uid e `mod` 6) + 1        
        entryClass = fromString $ "entry color_" ++ show modulo
        bodyClass = if tagless then "body tagless" else "body"
        self t = a ! href (permalink (baseUrl cfg) e) $ t
        readMoreBlock = do
            br
            "[" ; self "read more" ; "]"
        taglink t   = do
            preEscapedText "&nbsp;"
            a ! mkref cfg ("/page/" ++ t) ! class_ "colored" $ fromString $ prettifyTag t
        seriesLinksBlock =
            case listToMaybe $ M.seriesLinks e of
                 Nothing -> return ()
                 Just (M.SL first prev next last) -> H.div ! class_ "series-links" $ do
                     a ! mkref cfg (toString first) $ "First in series"
                     " | "
                     sLink "Previous" prev
                     " | "
                     sLink "Next" next
                     " | "
                     a ! mkref cfg (toString last) $ "Last in series"
        sLink :: Html -> Maybe M.Permalink -> Html
        sLink txt Nothing = txt
        sLink txt (Just v) = a ! mkref cfg (toString v) $ txt

-- | Produces a complete page with single entry.
layoutEntry :: RenderData M.SingleEntry -> Html
layoutEntry = layoutCommon layoutEntryBarebone

-- | Produces a complete page with multiple entries
layoutPage :: RenderData M.Page -> Html
layoutPage = layoutCommon inner
    where
        inner cfg entity = do
            mapM_ (layoutEntryBarebone cfg) $ M.entries entity
            let navi f css txt = case f entity of
                    Nothing -> return ()
                    Just h -> H.div ! class_ css $ a ! mkref cfg h $ txt
            H.div ! class_ "navi-container" $ do
                navi M.previous "navi previous" "Previous page"
                navi M.next "navi next" "Next page"

layoutNotFound :: RenderData () -> Html
layoutNotFound = layoutCommon inner
    where
        inner _ _ =
            H.div ! class_ "entry" $
                H.div ! class_ "body headless" $
                    "Can't find the page you've been looking for"

-- | Concatenates the urls.
urlConcat :: BaseURL -> String -> String
urlConcat base ('/':xs) = toString base ++ xs
urlConcat base xs = toString base ++ xs

-- | Renders the RSS feed.
renderFeed :: Local -> [M.RssEntry] -> String
renderFeed w items = showTopElement $ xmlRSS feed
    where
        base = baseUrl w
        feed :: RSS
        feed = RSS "2.0" [] channel []
        channel :: RSSChannel
        channel = (nullChannel (toString $ siteTitle w) (toString base))
            {rssDescription = "/dev/brain >> /dev/null"
            ,rssItems = Prelude.map convertItem items
            }
        convertItem :: M.RssEntry -> RSSItem
        convertItem x = (nullItem "")
            {rssItemTitle = toString <$> (emptyToNothing $ M.title x)
            ,rssItemLink = Just $ urlConcat base $ toString $ M.permalink x
            ,rssItemDescription = Just $ toString $ M.summary x
            ,rssItemGuid = Just $ RSSGuid
                {rssGuidPermanentURL = Just False
                ,rssGuidAttrs = []
                ,rssGuidValue = toString $ M.md5sig x
                }
            ,rssItemPubDate = Just $ toFeedDateStringUTC (RSSKind $ Just "2.0") $ M.timestamp x
            }

renderEntry :: RenderData M.SingleEntry -> Text
renderEntry = renderHtml . layoutEntry

renderPage :: RenderData M.Page -> Text
renderPage = renderHtml . layoutPage

renderNotFound :: RenderData () -> Text
renderNotFound = renderHtml . layoutNotFound
