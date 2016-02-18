module Anticore.Control.Flip where

import Control.Applicative(Applicative,liftA,pure,(<$>))
import Control.Monad(join,liftM)

class Flip a where
    flipA :: (Applicative m) => a (m b) -> m (a b)
    flipM :: (Monad m) => a (m b) -> m (a b)

instance Flip Maybe where
    flipA = maybe (pure Nothing) (liftA Just)        
    flipM = maybe (return Nothing) (liftM Just)    

reduceM :: (Monad m, Monad f, Flip f) => f (m (f a)) -> m (f a)
reduceM = liftM join . flipM

reduceM2 :: (Monad m, Monad f, Flip f) => m (f (m (f a))) -> m (f a)
reduceM2 = join . liftM reduceM

infixr 4 <$$>
(<$$>) :: (Functor m, Functor f) => (a -> b) -> m (f a) -> m (f b)
f <$$> x = fmap f <$> x

infixr 4 <!!>
(<!!>) :: (Functor m, Monad m, Functor f, Monad f, Flip f) => (a -> m (f b)) -> m (f a) -> m (f b)
f <!!> x = reduceM2 (f <$$> x)

promO :: (Monad m) => (a -> f b) -> a -> m (f b)
promO f x = return (f x)

promI :: (Monad f, Functor m) => (a -> m b) -> a -> m (f b)
promI f x = return <$> f x
