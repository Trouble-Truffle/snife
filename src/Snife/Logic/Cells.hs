-- | Controls the behavior of cells
module Snife.Logic.Cells where

import Data.Sequence

data Z a = Z {
      _ZList :: Seq a
   ,  _ZContent :: a
   ,  _ZIndex :: Int
   }

newtype ZZ a = ZZ {_unZZ :: Z (Z a)}  
data AliveCell = Snake | Food
data DeadCell = Dead | Transition
data CellState = AliveCell | DeadCell
