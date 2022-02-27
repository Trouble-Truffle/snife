-- | Handles the events
module Snife.Logic.EventHandler where

import           Brick
import           Graphics.Vty
import           Snife.AppTypes
import           Snife.Input
import           Snife.Logic.State

-- | Handles what to do every tick
eventHandler :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
eventHandler g (AppEvent Tick) = continue $ step g
eventHandler g (VtyEvent (EvKey (KChar 'q') [])) = halt g
eventHandler g (VtyEvent (EvKey key modifier)) = inputHandler key modifier g
eventHandler g _ = continue g
