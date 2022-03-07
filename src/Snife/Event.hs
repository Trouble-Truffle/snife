module Snife.Event where

import Brick 
import Snife.Types
import Control.Lens
import Graphics.Vty

eventHandler :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
eventHandler game (VtyEvent (EvKey (KChar 'q') [])) = halt game
eventHandler game event = continue $ game & score %~ (+1)

