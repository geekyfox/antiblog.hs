module Utils.Data.Outcome where

import Control.Applicative
import Control.Monad(liftM)
import Utils.Control.Flip

-- | Universal \"Maybe on steroids\" type for calculations that
--   may either: return value, signal the failure to obtain value,
--   or signal that value is \"not interesting\".
--
--   E.g. a text parser distinguishes situations when text file is
--   \"structured enough\" to have a syntax error (that's `Fail`)
--   and when text file is not in a supported format at all
--   (that's `Skip`).
data Outcome a = 
    -- | Result value.
    OK a 
    -- | Failed to obtain value because of particular reason.
  | Fail String
    -- | Depending on context, it's might be \"no action required\" or
    --   \"no action taken\" because of particular reason.
  | Skip String
  
instance Monad Outcome where
    Skip msg >>= _ = Skip msg
    Fail msg >>= _ = Fail msg
    OK x >>= f = f x
    return = OK
    fail   = Fail

instance Functor Outcome where
    fmap = liftM

instance Flip Outcome where
    flipA (OK x) = liftA OK x
    flipA (Fail msg) = pure $ Fail msg
    flipA (Skip msg) = pure $ Skip msg
    flipM (OK x) = liftM OK x
    flipM (Fail msg) = return $ Fail msg
    flipM (Skip msg) = return $ Skip msg

fromMaybe :: (Monad m) => String -> Maybe a -> m a
fromMaybe errmsg = maybe (fail errmsg) return

-- | Makes a string representation of `Outcome` using provided
--   function to render `OK` values.
describe :: (a -> String) -> Outcome a -> String
describe f = either id f . toEither

instance (Show a) => Show (Outcome a) where
    show = describe show

-- | Converts `Outcome` into either wrapped value or error message.
toEither :: Outcome a -> Either String a
toEither (OK x) = Right x
toEither (Fail msg) = Left $ "FAIL: " ++ msg
toEither (Skip msg) = Left $ "SKIP: " ++ msg

allOK :: [Outcome a] -> Outcome [a]
allOK = impl []
    where impl acc [] = OK $ reverse acc
          impl acc (x:xs) = case x of
                                 (OK y) -> impl (y:acc) xs
                                 (Fail msg) -> Fail msg
                                 (Skip _) -> impl acc xs

-- | Either returns a wrapped value or prints out an error message
--   and terminates the execution.
exposeOrDie :: Outcome a -> a
exposeOrDie = either error id . toEither

readInt :: String -> Maybe Int
readInt s = case reads s of
                 [(num, "")] -> Just num
                 _           -> Nothing
