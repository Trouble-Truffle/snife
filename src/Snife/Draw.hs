{-# LANGUAGE LambdaCase #-}
module Snife.Draw where

import Snife.Types
import Brick
import Brick.Widgets.Center

import Control.Lens
drawUI :: Game -> [Widget Name]
drawUI game = [center $ renderCells $ game ^. board]

renderCells :: Board -> Widget Name
renderCells = undefined

