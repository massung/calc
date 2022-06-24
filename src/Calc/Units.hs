{-# LANGUAGE OverloadedStrings #-}

module Calc.Units where

import Data.Foldable as F
import Data.List as L
import Data.Map as M

newtype Unit = Unit String
  deriving (Eq, Ord)

instance Show Unit where
  show (Unit u) = u

newtype Units = Units (Map Unit Integer)
  deriving (Eq)

instance Show Units where
  show (Units u) =
    case (F.null num, F.null den) of
      (False, False) -> expNum num ++ "/" ++ expNum den
      (False, _) -> expNum num
      (_, False) -> expDen den
      _ -> ""
    where
      (num, den) = L.partition ((> 0) . snd) $ M.toList u

      --
      showUnits (u, 1) = show u
      showUnits (u, n) = show u ++ "^" ++ show n

      --
      showUnitsAbs (u, 1) = show u
      showUnitsAbs (u, -1) = show u
      showUnitsAbs (u, n) = show u ++ "^" ++ show (abs n)

      -- show the numerator
      expNum = unwords . L.map showUnitsAbs
      expDen = unwords . L.map showUnits

fromList = Units . M.fromListWith (+)

singleton u = Units $ M.singleton u 1

recipUnits (Units a) =
  Units $ M.map negate a

multiplyUnits (Units a) (Units b) =
  Units $ M.filter (/= 0) $ M.unionWith (+) a b

divideUnits a b =
  multiplyUnits a $ recipUnits b
