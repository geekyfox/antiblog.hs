{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.Data.Tagged where

import Data.Maybe(fromMaybe,maybe)
import Data.String(IsString,fromString)

-- | Typeclass for `newtype`s around `String`s used to make operation
--   with multiple different strings more typesafe.
class ToString w where
    -- | Exposes string content
    toString :: w -> String

instance (ToString a, ToString b) => ToString (Either a b) where
    toString = either toString toString

instance ToString String where
    toString = id
    
instance ToString a => ToString (Maybe a) where
    toString = maybe "" toString

liftT :: (ToString a, IsString b) => (String -> String) -> a -> b
liftT f = fromString . f . toString

-- | Adapter between custom `TaggedString` and common `IsString`.
shapeshift :: (ToString a, IsString b) => a -> b
shapeshift = liftT id

emptyToNothing :: ToString a => a -> Maybe a
emptyToNothing x | null (toString x) = Nothing | otherwise = Just x

nothingToEmpty :: IsString a => Maybe a -> a
nothingToEmpty = fromMaybe (fromString "")
