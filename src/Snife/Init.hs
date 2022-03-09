
{-# LANGUAGE LambdaCase #-}
module Snife.Init where

import Brick
import Data.Sequence
import Control.Concurrent.STM.TVar


import Snife.Types
import Snife.Draw
import Snife.Event
import Snife.Theme
import Data.Zipper
app :: App Game Tick Name
app = App {
    appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = eventHandler
  , appStartEvent = return
  , appAttrMap = themeGenerator
  }

sampleBoard = matrixToGZip $ (map . map) (\case ' ' -> Dead; _ -> Alive)  
       [
            "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                           X                                              "
          , "                                         X X                                              "
          , "                               XX      XX             XX                                  "
          , "                              X   X    XX             XX                                  "
          , "                   XX        X     X   XX                                                 "
          , "                   XX        X   X XX    X X                                              "
          , "                             X     X       X                                              "
          , "                              X   X                                                       "
          , "                               XX                                                         "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                       XX                                 "
          , "                                                       XX                                 "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          , "                                                                                          "
          ]
initialGame :: TVar Int -> IO Game
initialGame tvar =  return Game {
    _board = sampleBoard
  , _speed = 1
  , _pause = True
  , _tval = tvar

  , _snake = singleton (Head, (0,0))
  , _cells = singleton (Alive,(0,0))
  , _cellSource = undefined

  , _frozen = False
  , _diet = False

  , _score = 0
  , _difficulty = Easy
  , _difTheme = EasyT
  , _lenTheme = Early
  , _debug = DebugInfo False 0
  }
