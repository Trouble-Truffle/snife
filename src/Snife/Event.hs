module Snife.Event where

import Brick 
import Snife.Types
import Control.Lens
import Graphics.Vty
import Control.Concurrent.STM.TVar

import Control.Monad.IO.Class
import GHC.Conc.Sync

import Control.Comonad
import Data.Zipper
import Snife.Util

eventHandler :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
eventHandler game (VtyEvent (EvKey (KChar 'q') [])) = halt game
eventHandler game (VtyEvent (EvKey (KChar 'w') [])) = handleSpeed game (-)
eventHandler game (VtyEvent (EvKey (KChar 's') [])) = handleSpeed game (+)
eventHandler game (AppEvent Tick) = continue $ game & board %~ step
eventHandler game event = continue $ game & score %~ (+1)

step :: Board -> Board
step = co_bind rule
  where
    living = length . filter (==Alive) . neighbors
    rule zipper = case (co_return zipper, living zipper) of
      (Alive, 2) -> Alive
      (Alive, 3) -> Alive
      (Dead , 3) -> Alive
      _ -> Dead

handleSpeed :: Game -> (Float -> Float -> Float) -> EventM Name (Next Game)
handleSpeed game (+/-) = do
  let newSpeed = (game^.speed) +/- speedIncrement
  liftIO $ atomically $ writeTVar (game^.tval) (floatToDelay newSpeed)
  continue $ game & speed .~ newSpeed
