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
import GHC.Exts
import Control.Applicative
import Control.Lens (from)

class Foldable l => Loopable l where
  data Direction l
  type Index l

  loopIndex :: l a -> Index l -> a
  shift :: Direction l -> l a -> l a

  -- | Does one permutaion for every shift 
  neighbors :: l a -> l (l a)

  -- | Gets all possible shifts
  permutations :: l a -> l (l a)


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

  neighbors = sequence [shift L, shift R]

  permutations seq = S.fromFunction (S.length seq) (\x -> composeN x (shift L) seq)
    where
      composeN n = foldr (.) id . replicate n

data Matrix a = Matrix { fromMatrix :: Seq (Seq a)} | NullMatrix

instance Show a => Show (Matrix a) where
  show NullMatrix = "| |"
  show matrix     = unlines $ F.concatMap addLine $ fmap (F.concat . S.intersperse "|")
                                              (fromMatrix $ fmap respace matrix)
   where

    addLine :: String -> [String]
    addLine x = [x >> "-", x]

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

instance Applicative Matrix where
  pure = Matrix . S.singleton . S.singleton
  liftA2 _ a NullMatrix = NullMatrix
  liftA2 _ NullMatrix b = NullMatrix
  liftA2 f a b = Matrix $ (S.zipWith . S.zipWith) f (fromMatrix a) (fromMatrix b)

instance Monad Matrix where
  NullMatrix >>= f = NullMatrix
  (Matrix x) >>= f = foldr (|^|) mempty
    $ fmap (F.foldr (<>) mempty . fmap f) x

instance Traversable Matrix where
  traverse f xs = Matrix <$> traverse sequenceA (fromMatrix $ fmap f xs)

instance IsList (Matrix a) where
  type Item (Matrix a) = a
  fromList [] = NullMatrix
  fromList xs = Matrix $ S.singleton $ S.fromList xs

  toList NullMatrix = []
  toList (Matrix xs) = F.toList $ F.foldr (<>) S.empty xs

instance Loopable Matrix where
  data Direction Matrix = N | E | S | W
  type Index Matrix = (Int, Int)

  loopIndex matrix (x, y) = loopIndex (loopIndex (fromMatrix matrix) y) x
  shift N = Matrix . shift L . fromMatrix
  shift S = Matrix . shift R . fromMatrix
  shift E = Matrix . fmap (shift R) . fromMatrix
  shift W = Matrix . fmap (shift L) . fromMatrix

  neighbors NullMatrix = return NullMatrix
  neighbors xs = sequence [shift N, id, shift S] =<< sequence [shift W, id, shift E] xs

  permutations NullMatrix = return NullMatrix
  permutations (Matrix xs)= Matrix xss
    where
      xss = (fmap . fmap) Matrix $ permutations <$> fmap permutations xs

horizontalConcat, verticalConcat :: Matrix a -> Matrix a -> Matrix a
horizontalConcat = (<>)
verticalConcat NullMatrix d = d
verticalConcat u NullMatrix = u
verticalConcat (Matrix u) (Matrix d) = Matrix $ u <> d

sampleMatrix :: Matrix Int
sampleMatrix = Matrix $ S.fromList [
    S.fromList [1,2,3]
  , S.fromList [1,2,3]
  , S.fromList [1,2,3]
  , S.fromList [4,5,6]
  , S.fromList [7,8,9]
  ]

-- | Add a character to every left side of the matrix
(|:|) :: a -> Matrix a -> Matrix a
x |:| NullMatrix = pure x
x |:| (Matrix xs) = Matrix $ fmap (x<|) xs

-- | Does a horizontal concatination
(<||) :: Matrix a -> Matrix a -> Matrix a
(<||) = (<>)

-- | Does a vertical concatination
(|^|) :: Matrix a -> Matrix a -> Matrix a
(|^|) NullMatrix b = b
(|^|) a NullMatrix = a
(|^|) a b = Matrix $ fromMatrix a <> fromMatrix b

merge :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
merge _ NullMatrix NullMatrix = NullMatrix
merge _ NullMatrix mat_R  = mat_R
merge _ mat_L NullMatrix = mat_L
merge oper mat_L mat_R = liftA2 oper mat_L mat_R

