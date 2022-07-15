module Calc.Foo where

import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M

newtype Foo a = Foo (Map a Integer)
  deriving (Eq, Ord)

instance Show a => Show (Foo a) where
  show (Foo xs)
    | F.null num = showExps den
    | F.null den = showExps num
    | otherwise = showExps num ++ "/" ++ showExps (M.map abs den)
    where
      (num, den) = M.partition (> 0) xs

      -- display a single unit with exponent
      showExp (x, 1) = show x
      showExp (x, n) = show x ++ "^" ++ show n

      -- concatenate units together
      showExps = unwords . L.map showExp . M.toList

instance Ord a => Semigroup (Foo a) where
  (<>) (Foo x) (Foo y) = Foo $ M.filter (/= 0) $ unionWith (+) x y

nullFoo (Foo xs) = F.null xs
