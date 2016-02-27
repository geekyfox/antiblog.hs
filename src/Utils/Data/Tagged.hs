{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.Data.Tagged where

import Data.Maybe(fromMaybe)
import Data.String(IsString,fromString)

-- | Typeclass for `newtype`s around `String`s used to make operation
--   with multiple different strings more typesafe.
class TaggedString w where
    -- | Exposes string content
    expose :: w -> String
    -- | Wraps string content
    wrap   :: String -> w
    
-- | Adapter between custom `TaggedString` and common `IsString`.
shapeshift :: (TaggedString a, IsString b) => a -> b
shapeshift = fromString . expose

liftT :: (TaggedString a) => (String -> String) -> a -> a
liftT f = wrap . f . expose

nonEmpty :: (TaggedString a) => a -> Maybe a
nonEmpty x = case expose x of { "" -> Nothing ; txt -> Just $ wrap txt }

empty :: (TaggedString a) => Maybe a -> a
empty = fromMaybe (wrap "")

instance TaggedString String where
    expose = id
    wrap = id
