module Snife.Draw where

import Snife.Types
import Brick
import Brick.Widgets.Center

import Control.Lens
drawUI :: Game -> [Widget Name]
drawUI game = [center $ str $ show $ game ^. score]
