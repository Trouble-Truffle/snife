{-# LANGUAGE TypeFamilies, TemplateHaskell, TypeApplications, FlexibleInstances, OverloadedLists #-}
module Data.Zipper where

import qualified Data.Sequence as S
import Data.Sequence (ViewL(..), ViewR(..), viewl, viewr, (<|), (|>))
import Control.Lens hiding ((:>), (:<), (|>), (<|))
import Control.Comonad

import Snife.Util

import Control.Monad
import Data.Foldable
import Data.List



class Zipper z where
  data Direction z

  shift :: Direction z -> z a -> z a
  neighbors :: z a -> [a]

-- Looped Zip <Begin> -----------------------------------
data LoopedZip a = LoopedZip {
    _focus :: a
  , _conts :: S.Seq a
  }
makeLenses ''LoopedZip
instance Show a => Show (LoopedZip a)  where
  show (LoopedZip foc conts) = ("[" ++ show foc ++ "]") ++ concat ( toList $ S.intersperse "|" $ fmap show conts)

instance Zipper LoopedZip where

  data Direction LoopedZip = L | R deriving (Show)

  shift direction (LoopedZip foc conts) =
    if null conts
      then LoopedZip foc conts
      else case direction of
        L -> LoopedZip x (xs |> foc)
        R -> LoopedZip y (foc <| ys)
    where
      (x :< xs) = viewl conts
      (ys :> y) = viewr conts

  neighbors (LoopedZip _ conts)
    | S.length conts <=   2 = toList conts
    | otherwise = map (S.index conts) [0, S.length conts - 1]


instance Functor LoopedZip where
  fmap f (LoopedZip foc conts) = LoopedZip (f foc) (fmap f conts)

instance Comonad LoopedZip where
  co_return  = _focus
  co_join zipper = LoopedZip zipper $
                  S.fromFunction (pred $ S.length $ zipper ^. conts)
                    (\k -> succ k `composeN` shift L $ zipper)
-- Looped Zip <End> -----------------------------------

-- Grid Zip <Begin> -------------------------------------
newtype GridZip z a = GridZip { _unZip :: z (z a) }
makeLenses ''GridZip

type GZ = GridZip LoopedZip

instance Show a => Show (GZ a) where
  show (GridZip lz) = (\(LoopedZip foc conts) -> unlines $ ("<" ++ foc ++ ">") : toList conts)
    $ fmap show lz

instance Zipper GZ where
  data Direction GZ = N | E | S | W
  shift E = (& unZip %~ shift L)
  shift W = (& unZip %~ shift R)
  shift S = (& unZip %~ fmap (shift L))
  shift N = (& unZip %~ fmap (shift R))
  neighbors (GridZip zipper) = neighbors (zipper^.focus) ++ concatMap neighbors' (neighbors zipper)
    where
    neighbors' z = (z^.focus) : neighbors z

matrixToGZip :: [[a]] -> GZ a
matrixToGZip = GridZip . g . map g
  where 
    g [] = error "Empty matrix"
    g (x:xs) = LoopedZip x $ S.fromList xs
-- Grid Zip <End> -------------------------------------


