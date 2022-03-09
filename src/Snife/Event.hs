{-# LANGUAGE LambdaCase #-}
module Snife.Event where

import Brick
import Snife.Types
import Control.Lens
import Graphics.Vty
import Control.Concurrent.STM.TVar
import Brick.Util

import Control.Monad.IO.Class
import GHC.Conc.Sync

import Control.Monad
import Control.Comonad
import Data.Zipper
import Snife.Util

eventHandler :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
eventHandler game (VtyEvent (EvKey (KChar 'q') [])) = halt game
eventHandler game (VtyEvent (EvKey (KChar 't') [])) = continue $ game & debug %~ draw %~ not
eventHandler game (VtyEvent (EvKey (KChar 'w') [])) = handleSpeed game (-)
eventHandler game (VtyEvent (EvKey (KChar 's') [])) = handleSpeed game (+)
eventHandler game (AppEvent Tick) = continue $ game & board %~ step
eventHandler game event = continue $ game & score %~ (+1)

-- | equivalent to the one line APL Game of Life
step :: Board -> Board
step board' = fmap (\case; True -> Alive; _ -> Dead)
  . foldr1 (merge (||))
  . zipWith ($) [id, merge (&&) (fmap toEnum board)]
  . sequence [fmap (==3), fmap (==4)]
  . foldr1 (merge (+))
  . join
  . map (sequence [shift N, id, shift S])
  . sequence [shift E, id, shift W]  $ board
  where
    board = (\case; Alive -> 1; _ -> 0) <$> board'

handleSpeed :: Game -> (Float -> Float -> Float) -> EventM Name (Next Game)
handleSpeed game (+/-) = do
  let newSpeed = (\x -> if x <= 0.1 then 0.1 else x ) (game^.speed) +/- speedIncrement
  liftIO $ atomically $ writeTVar (game^.tval) (floatToDelay newSpeed)
  continue $ game & speed .~ newSpeed
