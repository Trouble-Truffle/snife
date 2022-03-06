module Snife.Init where

import Brick

import Data.Sequence

import Snife.Types
import Snife.Draw
import Snife.Event
import Snife.Theme

app :: App Game Tick Name
app = App {
    appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = eventHandler
  , appStartEvent = return
  , appAttrMap = themeGenerator
  }

initialGame :: Game
initialGame =  Game {
    _board = [[]]
  , _speed = 1
  , _pause = True

  , _snake = singleton (Head, (0,0))
  , _cells = singleton (Alive,(0,0))
  , _cellSource = undefined

  , _frozen = False
  , _diet = False

  , _score = 0
  , _difficulty = Easy
  , _difTheme = EasyT
  , _lenTheme = Early
  }
