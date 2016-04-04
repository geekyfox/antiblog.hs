{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.Data.Tagged where

import Data.Maybe(fromMaybe)
import Data.String(IsString,fromString)

class ToString w where
    -- | Exposes string content
    expose :: w -> String

instance (ToString a, ToString b) => ToString (Either a b) where
    expose = either expose expose

instance ToString String where
    expose = id

-- | Typeclass for `newtype`s around `String`s used to make operation
--   with multiple different strings more typesafe.
class ToString w => TaggedString w where
    -- | Wraps string content
    wrap   :: String -> w
    
-- | Adapter between custom `TaggedString` and common `IsString`.
shapeshift :: (ToString a, IsString b) => a -> b
shapeshift = fromString . expose

liftT :: (ToString a, TaggedString b) => (String -> String) -> a -> b
liftT f = wrap . f . expose

nonEmpty :: (TaggedString a) => a -> Maybe a
nonEmpty x = case expose x of { "" -> Nothing ; txt -> Just $ wrap txt }

empty :: (TaggedString a) => Maybe a -> a
empty = fromMaybe (wrap "")

instance TaggedString String where
    wrap = id
