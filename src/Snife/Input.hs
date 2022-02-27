-- | Control the input output capabilities of the program
module Snife.Input where

import           Brick
import           Control.Monad.IO.Class
import           Graphics.Vty
import           Snife.AppTypes
import           Snife.Init
import           Snife.Logic.Snake

inputHandler :: Key -> [Modifier] -> Game -> EventM Name (Next Game)
inputHandler key modifier = case key of
  KEsc         -> inputHandler (KChar 'q') modifier

  KUp          -> inputHandler (KChar 'w') modifier
  KLeft        -> inputHandler (KChar 'a') modifier
  KDown        -> inputHandler (KChar 's') modifier
  KRight       -> inputHandler (KChar 'd') modifier

  (KChar key') -> case key' of
    'r' -> const $ liftIO initialGame >>= continue    -- Restart game
    'q' -> halt
    _ | key' `elem` "wj" -> continue . turn North
      | key' `elem` "ah" -> continue . turn West
      | key' `elem` "sk" -> continue . turn South
      | key' `elem` "dl" -> continue . turn East
      | otherwise        -> continue

  _ -> undefined
