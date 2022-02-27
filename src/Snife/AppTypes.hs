{-# LANGUAGE TemplateHaskell #-}

-- | Handles the types used throughout the program
module Snife.AppTypes where
import           Brick.Themes
import           Data.Sequence.Internal
import           Lens.Micro.TH
import           Linear.V2
type Name = ()


data Tick = Tick
  deriving Show

data Stream a = a :| Stream a -- ^ Used for infinite lists
type Coord = V2 Int

data Direction = North | East | South | West deriving (Show, Eq)

type Snake = Seq Coord  -- ^ Finite sequence of coordinates

data Game = Game
  { _snake     :: Snake -- ^ A sequence of coordinates showing all positions taken by the snake
  , _dir       :: Direction -- ^ N E S W compass directions
  , _foodCells :: Seq Coord -- ^ Coordinates of food cells
  , _foods     :: Stream (Seq Coord) -- ^ An infinite list of food spawns per tick
  , _theme     :: Theme
  , _dead      :: Bool
  , _paused    :: Bool
  , _frozen    :: Bool -- ^ to disallow duplicate turns and a small timeframe between moves
  , _score     :: Int
  , _time      :: Int
  , _speed     :: Float
  , _debug     :: String -- ^ some debugging info
  }
makeLenses ''Game
