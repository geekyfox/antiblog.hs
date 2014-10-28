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


module Parser where

import Data.Hash.MD5
import Control.Monad(foldM)
import Data.List(intercalate)
import Data.String.Utils(strip)

import Config(SystemName)
import qualified Model as M
import Utils

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
    deriving Eq

-- | Accumulator for parsed data.
data Parser = P {
      state     :: ParserState
    , system    :: SystemName
    , publish   :: Bool
    , title     :: Maybe M.Title
    , summary   :: Maybe M.Summary
    , uid       :: Maybe Int
    , body      :: [String]
    , footnotes :: [String]
    , symlink   :: Maybe M.Symlink
    , metalink  :: Maybe M.Metalink
    , tags      :: M.Tags
}

-- | Initializes a new parser.
mk :: SystemName -> Parser
mk sys = P {
      state     = Body
    , system    = sys
    , publish   = False
    , title     = Nothing
    , summary   = Nothing
    , uid       = Nothing
    , body      = []
    , footnotes = []
    , symlink   = Nothing
    , metalink  = Nothing
    , tags      = M.Tags []
}

-- | Returns full body text (composed of `body` and `footnotes`).
fullbody :: Parser -> M.Body
fullbody p = M.Body fullText
    where 
        mainPart = concat $ reverse $ body p
        footPart = intercalate "\n<br>" (reverse $ footnotes p)
        fullText
          | null footPart = mainPart
          | otherwise     = mainPart ++ "<hr/>" ++ footPart

-- | Calculates MD5 signature of the parsed entry.
signature :: Parser -> String
signature e = md5s $ Str $ concatMap (\f -> f e)
        [ maybe "" expose . title
        , maybe "" expose . summary
        ,          expose . fullbody
        , maybe "" expose . symlink
        , maybe "" expose . metalink
        ,          expose . tags
        ]

-- | Final stage of parsing, convert parser state into an `EntryFS`.
buildFS :: Parser -> Processed M.EntryFS
buildFS p
    | not $ publish p       = Skip "Not for publishing"
    | null $ expose content = Fail "Body is missing"
    | otherwise             = OK M.Entry {
              M.title    = title p
            , M.summary  = summary p
            , M.uid      = uid p
            , M.body     = content
            , M.symlink  = symlink p
            , M.metalink = metalink p
            , M.md5sig   = signature p
            , M.tags     = tags p
            }
  where
      content = fullbody p

-- | Appends new content line to active block.
append :: Parser -> String -> Parser
append p s =
    case state p of
         Body | null $ strip s -> p {
             body = "\n" : "</div><div class=\"stuff\">" : body p
         }
         Body -> p {
             body = "\n" : s : body p
         }
         Summary -> p {
             summary = Just $ wrap
                            $ maybe s (\x -> expose x ++ "\n" ++ s)
                            $ summary p
         }
         Footnote ->
             let (x:xs) = footnotes p
             in p { footnotes = (x ++ "\n" ++ s):xs}
         Code -> p {
             body = "\n" : s : body p
         }

-- | Inserts `summary` into `body`.
insertSummary :: Parser -> Processed Parser
insertSummary p | state p /= Body = Fail
    "insert-summary is allowed inside content block"
insertSummary p =
    case summary p of
         Nothing -> Fail "insert-summary directive without summary"
         Just s  -> OK $ append p $ expose s

-- | Starts accumulating new footnote.
startFootnote :: Parser -> Processed Parser
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
                        ("\n":xs) -> xs
                        xs        -> xs
        fmt nam ref = concat
                      [ "<a"
                      , " name = \"" ++ nam ++ index ++ "\""
                      , " href = \"" ++ ref ++ index ++ "\""
                      , "><sup>" ++ index ++ "</sup>"
                      , "</a>"
                      ]
        bodyPart  = fmt "tx" "#fn"
        footStart = fmt "fn" "#tx"

-- | Switches parser into `Body` state.
startBody :: Parser -> Parser
startBody p =
    case state p of
         Code -> p  { body  = "\n":"</pre>":body p
                    , state = Body
                    }
         _ -> p { state = Body }

-- | Switches parser into `Code` state.
startCode :: Parser -> Processed Parser
startCode p | state p /= Body = Fail
    "code is only allowed inside content block"
startCode p = OK p  { body  = "\n":"<pre>":body p
                    , state = Code
                    }

-- | Parses a single line of the text.
parseLine :: Parser -> String -> Processed Parser
parseLine p line =
    let
        impl ["draft"] =
            Skip "Draft"
        impl ["publish"] =
            OK p { publish = True }
        impl ["public", env, _] | env /= expose (system p) =
            OK p
        impl ["public", _, index] =
            case reads index of
                 [(num, "")] -> OK $ p { uid = Just num }
                 _           -> Fail $ "Bad ID: " ++ index
        impl ["symlink", link] =
            OK p { symlink = Just $ wrap link }
        impl ["meta", link] =
            OK p { metalink = Just $ wrap link }
        impl ("title":title) =
            OK p { title = Just $ wrap $ unwords title }
        impl ("tags":tags) =
            OK p { tags = M.Tags tags }
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
        impl _ = Fail $ "Unknown directive: " ++ line
    in case words line of
         ("##":"antiblog":rest) -> impl rest
         _ -> OK $ append p line

-- | Parses the whole text.
parseText :: SystemName -> [String] -> Processed M.EntryFS
parseText sys txt = foldM parseLine (mk sys) txt >>= buildFS
