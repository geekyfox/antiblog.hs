
module Anticore.Data.Tagged where

import Data.String(IsString,fromString)

-- | Typeclass for `newtype`s around `String`s used to make operation
--   with multiple different strings more typesafe.
class TaggedString w where
    -- | Exposes string content
    expose :: w -> String
    -- | Wraps string content
    wrap   :: String -> w
    -- | Adapter between custom `TaggedString` and common `IsString`.
    shapeshift :: (IsString a) => w -> a
    shapeshift = fromString . expose
