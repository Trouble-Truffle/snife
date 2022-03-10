{-# LANGUAGE TypeFamilies, ViewPatterns, OverloadedLists #-}

module Data.LoopList where
import           Control.Monad
import qualified Data.Foldable                 as F
import           Data.Function
import qualified Data.Sequence                 as S
import           Data.Sequence                  ( (<|)
                                                , (><)
                                                , Seq
                                                , ViewL(..)
                                                , ViewR(..)
                                                , (|>)
                                                )

class Foldable l => Loopable l where
  data Direction l
  type Index l

  loopIndex :: l a -> Index l -> a
  shift :: Direction l -> l a -> l a


instance Loopable Seq where

  type Index Seq = Int
  data Direction Seq = L | R

  loopIndex seq i = S.index seq (i `mod` S.length seq)

  shift dir seq = if S.null seq
    then seq
    else case dir of
      L -> xs |> x
      R -> y <| ys
   where
    (x  :< xs) = S.viewl seq
    (ys :> y ) = S.viewr seq


class Foldable m => Mergeable m where

  merge :: (a -> a -> a) -> m a -> m a -> m a

instance Mergeable Seq where
  merge = S.zipWith


data Matrix a = Matrix { fromMatrix :: Seq (Seq a)} | NullMatrix

instance Show a => Show (Matrix a) where
  show NullMatrix = "| |"
  show matrix     = unlines $ F.toList $ fmap (F.concat . S.intersperse "|")
                                              (fromMatrix $ fmap respace matrix)
   where

   -- Normalizes the size of all strings in the grid
    respace :: Show a => a -> String
    respace (show -> str) =
      let (lSpc, rSpc) = getLR $ length str
      in  replicate lSpc ' ' ++ str ++ replicate rSpc ' '

    getLR size = (\(a, b) -> (a, a + b)) $ (maxLen - size) `divMod` 2

    maxLen = maximum $ fmap (F.length . show) $ join $ fromMatrix matrix

instance Semigroup (Matrix a) where
  NullMatrix <> ml         = ml
  ml         <> NullMatrix = ml
  ml         <> mr         = Matrix $ (S.zipWith (><) `on` fromMatrix) ml mr

instance Monoid (Matrix a) where
  mempty = NullMatrix
  mappend x y = x <> y

instance Functor Matrix where
  fmap f x = Matrix (fmap f <$> fromMatrix x)

instance Foldable Matrix where
  foldr f x = foldr f x . join . fromMatrix

instance Loopable Matrix where
  data Direction Matrix = N | E | S | W
  type Index Matrix = (Int, Int)

  loopIndex matrix (x, y) = loopIndex (loopIndex (fromMatrix matrix) y) x
  shift N = Matrix . shift L . fromMatrix
  shift S = Matrix . shift R . fromMatrix
  shift E = Matrix . fmap (shift R) . fromMatrix
  shift W = Matrix . fmap (shift L) . fromMatrix

instance Mergeable Matrix where
  merge oper mat_L mat_R =
    Matrix $ ((merge . merge) oper `on` fromMatrix) mat_L mat_R

samplMatrix = Matrix [[1, 2, 3], [2, 3, 4], [4, 1, 6]]
