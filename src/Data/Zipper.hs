{-# LANGUAGE TypeFamilies, TemplateHaskell, TypeApplications#-}
module Data.Zipper where

import qualified Data.Sequence as S
import Data.Sequence (ViewL(..), ViewR(..), viewl, viewr, (<|), (|>))
import Control.Lens hiding ((:>), (:<), (|>), (<|))
import Control.Comonad

import Control.Monad
import Data.Foldable

import Snife.Util


class Zipper z where
  data Direction z

  shift :: Direction z -> z a -> z a
  neighbors :: z a -> [a]

data LoopedZip a = LoopedZip {
    _focus :: a
  , _conts :: S.Seq a
  } deriving (Show)
makeLenses ''LoopedZip

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






