-- | Controls the drawing of the application
module Snife.Draw where

import           Brick
import           Brick.Themes
import           Brick.Widgets.Center
import           Lens.Micro
import           Snife.AppTypes
import           Snife.Theming


drawUI :: Game -> [Widget Name]
drawUI gameState =
  [center $ padRight (Pad 2) (drawStats gameState) <=> drawGrid gameState]

drawStats, drawGrid :: Game -> Widget Name
drawStats g = str $ show $ g^.debug
drawGrid g = str "Snike prototype 1 (currently not snike)"

attrMap :: Game -> AttrMap
attrMap _ = themeToAttrMap theme1
