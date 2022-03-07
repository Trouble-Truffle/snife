{-# LANGUAGE TemplateHaskell #-}

module Snife.Types where

import Brick
import Control.Lens
import qualified Data.Sequence as S

data Tick = Tick

type Board = [[Int]]

-- | For infinite length sequences
data Stream a = a :| Stream a

-- | The Resource name
type Name = ()

type Coord = (Int,Int)

data SnakeSegment = Vertical | Horizontal | UpLeft | UpRight | DownLeft | DownRight | Head
type Snake = S.Seq (SnakeSegment, Coord)

data CellState = Alive | Dead | Transition | Snake deriving Show
type LiveCells = S.Seq (CellState, Coord)

data Difficulty = Easy | Medium | Hard

data SnifeDifTheme = EasyT | MediumT | HardT
data SnifeLenTheme = Early | Mid | Late


data Game =  Game {
    _board :: Board
  , _speed :: Float
  , _pause :: Bool

  , _snake :: Snake
  , _cells :: LiveCells
  , _cellSource :: Stream LiveCells -- Infinite list of cell spawns

  , _frozen :: Bool -- To disallow multiple turns at once
  , _diet :: Bool -- To disallow eating multiple consecutive cells

  , _score :: Int
  , _difficulty :: Difficulty
  , _difTheme :: SnifeDifTheme
  , _lenTheme :: SnifeLenTheme
  }

makeLenses ''Game
