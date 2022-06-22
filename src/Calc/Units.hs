{-# LANGUAGE OverloadedStrings #-}

module Calc.Units where

import Data.Foldable as F
import Data.List as L
import Data.Map as M
import Data.String

newtype Unit = Unit String
  deriving (Eq, Ord)

instance Show Unit where
  show (Unit u) = u

instance IsString Unit where
  fromString = Unit

newtype Units = Units (Map Unit Int)
  deriving (Eq)

instance Show Units where
  show (Units u) =
    case (F.null num, F.null den) of
      (False, False) -> showNum ++ "/" ++ showDen
      (False, _) -> showNum
      (_, False) -> showDen
      _ -> ""
    where
      showUnits (u, 1) = show u
      showUnits (u, -1) = show u
      showUnits (u, n) = show u ++ "^" ++ show (abs n)

      -- separate the units into the numerator and denominator
      (num, den) = L.partition ((> 0) . snd) $ M.toList u

      -- show the numerator
      showNum = intercalate "*" $ L.map showUnits num
      showDen = intercalate "*" $ L.map showUnits den

emptyUnits = Units M.empty

recipUnits (Units a) =
  Units $ M.map negate a

multiplyUnits (Units a) (Units b) =
  Units $ M.filter (/= 0) $ M.unionWith (+) a b

simplifyUnits (Units a) (Units b) = (Units a', Units b')
  where
    a' = M.filter (/= 0) $ M.unionWith (-) a $ M.intersection b a
    b' = M.filter (/= 0) $ M.unionWith (-) b $ M.intersection a b
