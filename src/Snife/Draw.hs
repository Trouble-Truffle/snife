
module Snife.Draw where

import Snife.Types
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import qualified Data.Sequence as S

import GHC.Conc.Sync
import Data.Zipper
import Control.Lens
import Data.Foldable
import System.IO.Unsafe

--drawUI game = [center $ str (("Delay: "++)$ take 4 $ show $ game^.speed) <=> drawGrid game]
drawUI :: Game -> [Widget Name]
drawUI game = [center ( str $ show $ unsafePerformIO $ readTVarIO $ game^.tval) <=> 
        (if game^.debug.draw
          then drawGrid game
          else str $ show $ game^.board)
      ]

drawGrid :: Game -> Widget Name
drawGrid game = border $ vBox xs
  where
    xs = mkCol ( brd^.unZip.focus) : toList (fmap mkCol (brd^.unZip.conts))

    mkCol :: LoopedZip CellState -> Widget Name
    mkCol = foldr ((<+>) . renderCell) emptyWidget . flatten

    rowSize = succ $ S.length $ brd^. unZip.conts
    brd = game^.board

renderCell :: CellState -> Widget Name
renderCell c = withAttr (attrName $ show c) $ str " "
