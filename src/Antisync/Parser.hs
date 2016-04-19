
{-# LANGUAGE OverloadedStrings #-}

-- | This is an implementation of parser for (largely ad hoc and
--   rudimentary) Antiblog's markup language.
--
--   Minimal valid document looks like this
--
--   >## antiblog publish
--   >Wise thought you want to share with the World.
--
--   More feature-rich document looks like
--
--   >## antiblog publish
--   >## antiblog symlink my-first-post
--   >## antiblog tags haskell test
--   >## antiblog meta test
--   >## antiblog summary
--   >Here's some text that will be visible on front page.
--   >## antiblog content
--   >Here's some preface.
--   >
--   >## antiblog insert-summary
--   >
--   >And here's a code snippet
--   >## antiblog footnote
--   >And a footnote too! Just because we can!
--   >## antiblog content
--   >.
--   >## antiblog code
--   >main = do
--   >    print "Hello, world"
--   >## antiblog content
--
--   Every \"command\" line starts with @\#\# antiblog@; every other
--   line is treated as content, check `ParserState` to see what
--   exactly happens to it.
--
--   Full list of directives that are implemented so far:
--
--   * @\#\# antiblog publish@
--
--     This file should be published. Publishing is on opt-in basis:
--     if this line is missing, entry won't be published.
--
--   * @\#\# antiblog draft@
--
--     This file contains a draft of the text and shouldn't be
--     published. Overrides @publish@ directive. Immediately aborts
--     parsing of the remainder of the file (thus, no breaking on
--     markup errors below this directive).
--
--   * @\#\# antiblog public \<server\> \<id\>@
--
--     This post is published on \<server\> (see `Config.systemName`).
--     Is automatically inserted by `antisync` after initial
--     posting of the entry. There may be multiple such entries if
--     you post to multiple webservers.
--
--   * @\#\# antiblog symlink \<link\>@
--
--     Symbolic link of the entry (see `Model.symlink`).
--
--   * @\#\# antiblog meta \<link\>@
--
--     Symbolic link with \"meta\" prefix (see `Model.metalink`).
--     I use it to mark the blog posts about the blog itself.
--
--   * @\#\# antiblog title \<space-separated words\>@
--
--     Title of the entry.
--
--   * @\#\# antiblog tags \<space-separated tags\>@
--
--     Tags of the entry.
--
--   * @\#\# antiblog summary@
--
--     Switches parser to `Summary` state.
--
--   * @\#\# antiblog footnote@
--
--     Switches parser to `Footnote` state.
--
--   * @\#\# antiblog code@
--
--     Switches parser to `Code` state.
--
--   * @\#\# antiblog insert-summary@
--
--     Inserts the content of `summary` block at current position
--     in `body`.
--
--   * @\#\# antiblog content@
--
--     Switches parser to `Body` state. Since it's a default state,
--     directive is mainly supposed to be used as a terminator for
--     @summary@, @footnote@ and @code@ blocks.
--

module Antisync.Parser where

import Control.Monad(foldM)
import Data.Hash.MD5
import Data.List(intercalate)
import Data.Maybe(isJust,fromJust)
import Data.String(fromString)
import Data.String.Utils(strip)

import Skulk.Outcome
import Skulk.ToString

import qualified Common.Model as M

import Antiblog.Config(SystemName)

readInt :: String -> Maybe Int
readInt s = case reads s of
                 [(num, "")] -> Just num
                 _           -> Nothing

-- | Current state of the parser that specifies what kind of
--   text is the current non-command line as well as what state
--   changes are allowed.
data ParserState =
    -- | Normal content. Initial state of the parser. Text line is
    -- appended to `body`.
    --
    --   It's permitted to switch to any other state.
      Body
    -- | Summary content. Text line is appended to `summary`.
    --
    --   The only legal next state is `Body`.
    | Summary
    -- | Footnote. Text line is appended to the last footnote.
    --   On entering state new empty footnote is added to `footnotes`
    --   and hyperlink to that footnote is added to `body`.
    --
    --   The only legal next state is `Body`.
    | Footnote
    -- | Code snippet. Text line is appended to `body`; on entering
    --   state \<pre\> tag is added to `body`; on leaving state
    --   \</pre\> tag is added to `body`.
    --
    --   The only legal next state is `Body`.
    | Code
    | Poem
    deriving Eq

data BodyLine = Raw String
              | Newline
              | Separator

-- | Accumulator for parsed data.
data Parser = P {
    state :: ParserState
    ,system :: SystemName
    ,publish :: Bool
    ,title :: M.Title
    ,summary :: M.Summary
    ,uid :: Maybe Int
    ,body :: [BodyLine]
    ,footnotes :: [String]
    ,symlink :: Maybe M.Symlink
    ,metalink :: Maybe M.Metalink
    ,tags :: [M.Tag]
    ,series :: [M.SeriesRef]
    ,redirect :: Maybe String
}

-- | Initializes a new parser.
mk :: SystemName -> Parser
mk sys = P {
    state = Body
    ,system = sys
    ,publish = False
    ,title = ""
    ,summary = ""
    ,uid = Nothing
    ,body = []
    ,footnotes = []
    ,symlink = Nothing
    ,metalink = Nothing
    ,tags = []
    ,series = []
    ,redirect = Nothing
}

trimWhite :: [BodyLine] -> [BodyLine]
trimWhite (Separator:xs) = trimWhite xs
trimWhite (Newline:xs) = trimWhite xs
trimWhite xs = xs

renderBody :: [BodyLine] -> [String]
renderBody = map convert . trimWhite . reverse . trimWhite
    where
        convert (Raw x)   = x
        convert Separator = "</div><div class=\"stuff\">"
        convert Newline   = "\n"

-- | Returns full body text (composed of `body` and `footnotes`).
fullbody :: Parser -> M.Body
fullbody p = M.Body fullText
    where 
        mainPart = concat $ renderBody $ body p
        footPart = intercalate "\n<br>" (reverse $ footnotes p)
        fullText
          | null footPart = mainPart
          | otherwise     = mainPart ++ "<hr/>" ++ footPart

-- | Calculates MD5 signature of the parsed entry.
signature :: Parser -> M.MD5Sig
signature e = M.MD5Sig $ md5s $ Str $ concatMap (\f -> f e) $ case redirect e of
    Just r ->
        [const r
        ,toString . symlink
        ,toString . metalink
        ]
    Nothing ->
        [toString . title
        ,toString . summary
        ,toString . fullbody
        ,toString . symlink
        ,toString . metalink
        ,intercalate ";" . map toString . tags
        ,concatMap show . series
        ]

-- | Final stage of parsing, convert parser state into an `EntryFS`.
buildFS :: Parser -> Outcome M.File
buildFS p
    | not $ publish p = Skip "Not for publishing"
    | isJust (redirect p) = case uid p of
        Nothing -> Fail "ID is missing in redirected entry"
        Just n -> OK $ M.Redirect $ M.Entry (M.StoredId n (signature p)) content extra
            where
                extra = M.RedirectExtra (symlink p) (metalink p)
                content = fromString $ fromJust $ redirect p
    | null $ toString $ fullbody p = Fail "Body is missing"
    | otherwise = OK $ case uid p of
            Nothing -> M.New (M.Entry (M.NewId (signature p)) content extra)
            Just n -> M.Stored (M.Entry (M.StoredId n (signature p)) content extra)
        where
            content = M.StoredContent (title p) (fullbody p) (summary p) (tags p)
            extra = M.StoredExtra (series p) (symlink p) (metalink p)

-- | Appends new content line to active block.
append :: Parser -> String -> Parser
append p s =
    case state p of
         Body | null $ strip s -> p {
             body = Newline : Separator : body p
         }
         Body -> p {
             body = Newline : Raw s : body p
         }
         Poem | null $ strip s -> p {
             body = Newline : Separator : body p
         }
         Poem -> p {
             body = Newline : Raw "<br />" : Raw s : body p
         }
         Summary -> p {
             summary = liftT (\x -> x ++ "\n" ++ s) $ summary p
         }
         Footnote ->
             let (x:xs) = footnotes p
             in p { footnotes = (x ++ "\n" ++ s):xs}
         Code -> p {
             body = Newline : Raw s : body p
         }

-- | Inserts `summary` into `body`.
insertSummary :: Parser -> Outcome Parser
insertSummary p | state p /= Body = Fail
    "insert-summary is allowed inside content block"
insertSummary p =
    case emptyToNothing (summary p) of
         Nothing -> Fail "insert-summary directive without summary"
         Just s  -> OK $ append p $ toString s

-- | Starts accumulating new footnote.
startFootnote :: Parser -> Outcome Parser
startFootnote p | state p /= Body = Fail
    "footnotes is only allowed inside content block"
startFootnote p = OK p
        { body = bodyPart:bodyTail
        , footnotes = footStart:footnotes p
        , state = Footnote
        }
    where 
        index    = show $ length (footnotes p) + 1        
        bodyTail = case body p of
                        (Newline:xs) -> xs
                        xs           -> xs
        fmt nam ref = concat
                      [ "<a"
                      , " name = \"" ++ nam ++ index ++ "\""
                      , " href = \"" ++ ref ++ index ++ "\""
                      , "><sup>" ++ index ++ "</sup>"
                      , "</a>"
                      ]
        bodyPart  = Raw $ fmt "tx" "#fn"
        footStart = fmt "fn" "#tx"

-- | Switches parser into `Body` state.
startBody :: Parser -> Parser
startBody p =
    case state p of
         Code -> p  { body  = Newline:Raw "</pre>":body p
                    , state = Body
                    }
         _ -> p { state = Body }

-- | Switches parser into `Code` state.
startCode :: Parser -> Outcome Parser
startCode p | state p /= Body = Fail
    "code is only allowed inside content block"
startCode p = OK p  { body  = Newline:Raw "<pre>":body p
                    , state = Code
                    }

addSeries :: Parser -> String -> Int -> Outcome Parser
addSeries p s ix = OK $ p {
        series = M.SeriesRef s ix : series p
    }

-- | Parses a single line of the text.
parseLine :: Parser -> String -> Outcome Parser
parseLine p line =
    let
        impl ["draft"] =
            Skip "Draft"
        impl ["publish"] =
            OK p { publish = True }
        impl ["public", env, _] | env /= toString (system p) =
            OK p
        impl ["public", _, index] =
            case readInt index of
                 Just num -> OK $ p { uid = Just num }
                 _        -> Fail $ "Bad ID: " ++ index
        impl ["symlink", link] =
            OK p { symlink = Just $ fromString link }
        impl ["meta", link] =
            OK p { metalink = Just $ fromString link }
        impl ("title":title) =
            OK p { title = fromString $ unwords title }
        impl ("tags":tags) =
            OK p { tags = map fromString tags }
        impl ["summary"] =
            OK p { state = Summary }
        impl ["content"] =
            OK $ startBody p
        impl ["footnote"] =
            startFootnote p
        impl ["insert-summary"] =
            insertSummary p
        impl ["code"] =
            startCode p
        impl ["poem"] =
            OK p { state = Poem }
        impl ["series", name, index] =
             case readInt index of
                  Just num -> addSeries p name num
                  _ -> Fail $ "Bad series index: " ++ index
        impl ["redirect", env, _] | env /= toString (system p) =
            OK p
        impl ["redirect", _, href] =
            OK p { redirect = Just href }
        impl _ = Fail $ "Unknown directive: " ++ line
    in case words line of
         ("##":"antiblog":rest) -> impl rest
         _ -> OK $ append p line

-- | Parses the whole text.
parseText :: SystemName -> [String] -> Outcome M.File
parseText sys txt = foldM parseLine (mk sys) txt >>= buildFS
