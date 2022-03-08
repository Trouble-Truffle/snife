module Control.Comonad where

class Functor w => Comonad w where
  co_return :: w a -> a

  co_bind :: (w a -> b) -> w a -> w b
  co_bind f = fmap f . co_join

  co_join :: w a -> w (w a)
  co_join = co_bind id

  {-# MINIMAL co_return, (co_join | co_bind) #-}

(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip co_bind
