
-- | Various utility functions.

module Utils where

import Control.Monad(liftM)
import Data.String(IsString,fromString)

-- | Equivalent to `fmap`/`liftM` with swapped parameters.
--   Useful for writing chains of data transformations left-to-right.
(|>>) :: (Monad f) => f a -> (a -> b) -> f b
infixl 1 |>>
x |>> f = f `liftM` x

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

-- | Universal \"Maybe on steroids\" type for calculations that
--   may either: return value, signal the failure to obtain value,
--   or signal that value is \"not interesting\".
--
--   E.g. `Parser` distinguishes situations when text file is
--   \"structured enough\" to have a syntax error (that's `Fail`)
--   and when text file is not in a supported format at all
--   (that's `Skip`).
data Processed a = 
    -- | Result value.
    OK a 
    -- | Failed to obtain value because of particular reason.
  | Fail String
    -- | Depending on context, it's might be \"no action required\" or
    --   \"no action taken\" because of particular reason.
  | Skip String

instance Monad Processed where
    Skip msg >>= _ = Skip msg
    Fail msg >>= _ = Fail msg
    OK x     >>= f = f x
    
    return = OK
    fail   = Fail

instance Functor Processed where
    fmap = liftM

instance (Show a) => Show (Processed a) where
    show = describe show

-- | Shorthand type.
type MProc m a = m (Processed a)
-- | Shorthand type.
type ProcM m a = Processed (m a)
-- | Shorthand type.
type IOProc a = MProc IO a
-- | Shorthand type.
type ProcIO a = ProcM IO a

-- | Utility function for combining `Processed` with some other monad
--   type.
transpose :: (Monad m) => ProcM m a -> MProc m a
transpose (OK x)     = liftM OK x
transpose (Fail msg) = return $ Fail msg
transpose (Skip msg) = return $ Skip msg

-- | Utility function for combining `Processed` with some other monad
--   type.
liftProc :: (Monad m) => (a -> m b) -> Processed a -> m (Processed b)
liftProc f = transpose . liftM f

-- | Converts `Processed` into either wrapped value or error message.
toEither :: Processed a -> Either String a
toEither (OK x)     = Right x
toEither (Fail msg) = Left $ "FAIL: " ++ msg
toEither (Skip msg) = Left $ "SKIP: " ++ msg

-- | Makes a string representation of `Processed` using provided
--   function to render `OK` values as-is.
describe :: (a -> String) -> Processed a -> String
describe f = either id f . toEither

-- | Monad-ish version of `describe`.
describeM :: (Monad m) => (a -> m String) -> Processed a -> m String
describeM f x = transpose (liftM f x) |>> describe id

-- | Either returns a wrapped value or prints out an error message
--   and terminates the execution.
exposeOrDie :: Processed a -> a
exposeOrDie = either error id . toEither
