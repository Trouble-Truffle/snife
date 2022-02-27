-- | Control the initial state of the program
module Snife.Init where

import Snife.AppTypes
import Snife.Theming
import Data.Sequence.Internal
import Linear.V2

--initialGame = Game
        --{ _snake  = (singleton (V2 2 2))
        --, _food   = f
        --, _foods  = fs
        --, _score  = 0
        --, _dir    = North
        --, _dead   = False
        --, _paused = True
        --, _locked = False
        --}

initialGame :: IO Game
initialGame = return $ Game
  { 
    _snake  = singleton $ V2 2 2
  , _dir    = North
  , _debug  = "hello"
  , _foodCells = singleton $ V2 2 2
  , _foods  = Snife.Init.fromList $ repeat $ singleton (V2 1 1)
  , _dead   = False
  , _paused = True
  , _score  = 0
  , _frozen = False
  , _time   = 0
  , _speed  = 1
  , _theme  = theme1
  }

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
