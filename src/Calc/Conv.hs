{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where

import Calc.Parser.Scalar
import Calc.Scalar
import Calc.Units
import Data.Map.Strict as M

data Conv = Conv {scalar :: Scalar, factor :: Integer}
  deriving (Eq, Show)

(>*>) Conv {scalar = a, factor = f} Conv {scalar = b, factor = g} =
  Conv {scalar = a * expScalar b f, factor = f * g}

conversions :: [(Units, Scalar)]
conversions = concatMap explode $ imperialConversions ++ siConversions
  where
    explode (from, tos) = [(from, to) | to <- tos]

imperialConversions =
  [ ("h", ["4 in"]),
    ("ft", ["12 in", "0.3048 m"]),
    ("yd", ["3 ft"]),
    ("ch", ["22 yd"]),
    ("fur", ["10 ch"]),
    ("mi", ["8 fur"]),
    ("lea", ["3 mi"]),
    ("ftm", ["2 yd"]),
    ("cable", ["100 ftm"]),
    ("nmi", ["10 cable"]),
    ("link", ["7.92 in"]),
    ("rod", ["25 link"]),
    ("acre", ["43560.04 ft^2"]),
    ("lb", ["16 oz", "453.5924 g"]),
    ("st", ["14 lb"]),
    ("qtr", ["28 lb"]),
    ("cwt", ["112 lb"]),
    ("t", ["2240 lb"]),
    ("slug", ["32.17404856 lb"]),
    ("min", ["60 s"]),
    ("hr", ["60 min"]),
    ("day", ["24 hr"]),
    ("bar", ["100000 Pa", "14.50377 psi"])
  ]

siConversions = [(fromUnit u, [conv u p x]) | u <- metricUnits, (_, p, x) <- siPrefixes]
  where
    conv u p x = let u' = unitMap ! (p ++ symbol u) in Scalar (recip x) $ Just (fromUnit u')
