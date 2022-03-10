{-# LANGUAGE ViewPatterns, LambdaCase #-}
module Snife.Draw where

import Snife.Types
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import qualified Data.Sequence as S
import Data.LoopList

import GHC.Conc.Sync
import Data.Zipper
import Control.Lens
import Data.Foldable
import System.IO.Unsafe

drawUI :: Game -> [Widget Name]
drawUI game = [center ( str $ show $ unsafePerformIO $ readTVarIO $ game^.tval) <=> 
        (if game^.debug.draw
          then drawGrid game
          else str $ show $ sum $ fmap (\case; Alive -> 1; _ -> 0) $ game^.board)
      ]

drawGrid :: Game -> Widget Name
drawGrid ((^.board)->brd) = border $ foldr ((<=>) . foldr (<+>) emptyWidget) emptyWidget $ fromMatrix $ fmap renderCell brd

renderCell :: CellState -> Widget Name
renderCell c = withAttr (attrName $ show c) $ str " "
