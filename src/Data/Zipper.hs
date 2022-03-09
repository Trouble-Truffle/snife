{-# LANGUAGE TypeFamilies, TemplateHaskell, TypeApplications, FlexibleInstances, OverloadedLists, LambdaCase #-}
module Data.Zipper where

import qualified Data.Sequence as S
import Data.Sequence (ViewL(..), ViewR(..), viewl, viewr, (<|), (|>))
import Control.Lens hiding ((:>), (:<), (|>), (<|), index)
import Control.Comonad

import Snife.Util

import Data.Tuple
import Control.Monad
import Data.Foldable
import Data.List



class Zipper z where
  data Direction z
  type Index z

  shift :: Direction z -> z a -> z a
  neighbors :: z a -> [a]
  flatten :: z a -> [a] -- Turn the content and focus into just one list
  (<!>) :: z a -> Data.Zipper.Index z -> a -- No fail state as the list is looping

-- Looped Zip <Begin> -----------------------------------
data LoopedZip a = LoopedZip {
    _focus :: a
  , _conts :: S.Seq a
  , _index :: Int
  }
makeLenses ''LoopedZip

instance Show a => Show (LoopedZip a)  where
  show (LoopedZip foc conts index) = ("  [" ++ show foc ++ "]") ++ concat ( toList $ S.intersperse "," $ fmap show conts) ++ ("(" ++ show index ++ ")")


instance Zipper LoopedZip where
  data Direction LoopedZip = L | R deriving (Show)
  type Index LoopedZip = Int

  shift direction zip@(LoopedZip foc conts index) =
    if null conts
      then zip
      else case direction of
        L -> LoopedZip x (xs |> foc) $ succ index `mod` size
        R -> LoopedZip y (foc <| ys) $ index `mod` size
    where
      size = 1 + S.length conts
      (x :< xs) = viewl conts
      (ys :> y) = viewr conts

  -- | Obtains neighbors by indexing both ends of a sequence
  neighbors (LoopedZip _ conts _)
    | S.length conts <=   2 = toList conts
    | otherwise = map (S.index conts) [0, S.length conts - 1]

  flatten (LoopedZip foc conts _) = foc : toList conts

  (LoopedZip foc conts _ ) <!> index'
    | index == 0 = foc
    | otherwise = conts `S.index` pred index
    where
      index = index' `mod` succ (S.length conts)


instance Functor LoopedZip where
  fmap f (LoopedZip foc conts i) = LoopedZip (f foc) (fmap f conts) i

instance Comonad LoopedZip where
  co_return  = _focus
  co_join zipper = LoopedZip
                    zipper
                    ( S.fromFunction (succ $ S.length $ zipper ^. conts)
                      (\k -> succ k `composeN` shift L $ zipper)
                    )
                    (zipper^.index)
-- Looped Zip <End> -----------------------------------

-- Grid Zip <Begin> -------------------------------------
newtype GridZip z a = GridZip { _unZip :: z (z a) }
makeLenses ''GridZip

type GZ = GridZip LoopedZip

instance Show a => Show (GZ a) where
  show (GridZip lz) = (\(LoopedZip foc conts i) -> unlines $ ("(" ++ show i ++ ")<" ++ foc ++ ">") : toList conts)
    $ fmap show lz

instance Zipper GZ where
  data Direction GZ = N | E | S | W
  type Index GZ = (Int, Int)

  shift E = (& unZip %~ shift L)
  shift W = (& unZip %~ shift R)
  shift S = (& unZip %~ fmap (shift L))
  shift N = (& unZip %~ fmap (shift R))
  neighbors (GridZip zipper) = neighbors (zipper^.focus) ++ concatMap neighbors' (neighbors zipper)
    where
    neighbors' z = (z^.focus) : neighbors z

  flatten (GridZip (LoopedZip foc conts _)) = flatten foc ++ concatMap flatten (toList conts)

  (GridZip zipper) <!> (xIndex,yIndex) = (<!> xIndex) $ zipper <!> yIndex


instance Functor GZ where
  fmap f = GridZip . (fmap . fmap) f . _unZip

instance Comonad GZ where
  co_return =  _focus . _focus . _unZip
  co_join zipper = GridZip $ LoopedZip
    (LoopedZip
      zipper
      (S.fromFunction (ySize - 1) (`mkRow` zipper))
      y
    )
    (S.fromFunction (xSize - 1) mkCol)
    x
    where
    mkRow j = composeN (j+1) (shift S)
    mkCol i = LoopedZip
                zx
                (S.fromFunction (pred ySize) (`mkRow` zx))
                (zx ^. (unZip . focus . index))
      where
        zx =  composeN (i+1) (shift E) zipper

    (xSize,ySize) = (succ . S.length $ zipper ^. (unZip . conts),
                     succ . S.length $ zipper ^. (unZip . focus . conts))
    (x,y) = (zipper ^. (unZip . index),
            zipper ^. (unZip . focus . index))

matrixToGZip :: [[a]] -> GZ a
matrixToGZip = GridZip . g . map g
  where
    g [] = error "Empty matrix"
    g (x:xs) = LoopedZip x (S.fromList xs) 0
-- Grid Zip <End> -------------------------------------

