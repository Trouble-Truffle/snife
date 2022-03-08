
module Snife.Draw where

import Snife.Types
import Brick
import Brick.Widgets.Center
import Data.Zipper
import Control.Lens
import Data.Foldable

drawUI :: Game -> [Widget Name]
drawUI game = [center (renderCells $ game ^. board)]

renderCells :: Board -> Widget Name
renderCells (GridZip (LoopedZip x y _)) = vBox 
  (
    hBox (map renderCell $ flatten x) 
    : toList (fmap (hBox . map renderCell . flatten) y)
  )


renderCell :: CellState -> Widget Name
renderCell c = withAttr (attrName $ show c) $ str " "
