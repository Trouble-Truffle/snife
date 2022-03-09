module Control.Comonad where

class Functor w => Comonad w where
  co_return :: w a -> a

  co_bind :: (w a -> b) -> w a -> w b
  co_bind f = fmap f . co_join

  co_join :: w a -> w (w a)
  co_join = co_bind id

  {-# MINIMAL co_return, (co_join | co_bind) #-}


-- | alias for `co_bind`
(<<=):: Comonad w => (w a -> b) -> w a -> w b
(<<=) = co_bind

-- | alias for `flip co_bind`
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip co_bind


-- | Alias for `co_join`
duplicate :: Comonad w => w a -> w (w a)
duplicate = co_join

-- | Alias for `co_return`
extract :: Comonad w => w a -> a
extract = co_return
