module Snife.Event where

import Brick 
import Snife.Types
import Control.Lens
import Graphics.Vty

import Control.Comonad

eventHandler :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
eventHandler game (VtyEvent (EvKey (KChar 'q') [])) = halt game
eventHandler game (AppEvent Tick) = undefined
eventHandler game event = continue $ game & score %~ (+1)

step :: Board -> Board
step = (=>> rule)
  where
    rule = undefined

