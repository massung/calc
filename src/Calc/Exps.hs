module Calc.Exps where

import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M

type Exps a = Map a Integer

showExps :: Show a => Exps a -> String
showExps xs
  | F.null num = showExps' den
  | F.null den = showExps' num
  | otherwise = showExps' num ++ "/" ++ showExps' (M.map abs den)
  where
    (num, den) = M.partition (> 0) xs

    -- display a single unit with exponent
    showExp (x, 1) = show x
    showExp (x, n) = show x ++ "^" ++ show n

    -- concatenate units together
    showExps' = unwords . L.map showExp . M.toList

appendExps :: Ord a => Exps a -> Exps a -> Exps a
appendExps x y = M.filter (/= 0) $ unionWith (+) x y

nullExps :: Exps a -> Bool
nullExps = F.null

simplify :: Exps a -> (Exps a, Integer)
simplify m = (M.map (`div` factor) m, factor)
  where
    factor =
      let x = M.foldl' gcd (maximum m) m
       in if all (< 0) m then negate x else x
