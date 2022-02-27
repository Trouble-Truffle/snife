-- | Controls current game state 
module Snife.Logic.State where

import           Control.Monad.Trans.State.Lazy

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe


import           Snife.AppTypes

step :: Game -> Game
step game = flip execState game . runMaybeT $ do

   -- Check the game variable if any stopping state is true
  MaybeT $ guard . not . or <$> sequence [use dead, use paused]

  -- Unlock from last turn
  MaybeT . fmap Just $ frozen .= False

  die <|> eatFood <|> move

die, eatFood, move :: MaybeT (State Game) ()

-- | Check if snake is in a death state
die = undefined

-- | Check if in eat state
eatFood = undefined

move = undefined
