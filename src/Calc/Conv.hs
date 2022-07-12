{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where

import Calc.Parser.Scalar
import Calc.SI
import Calc.Scalar
import Calc.Units
import Data.Map.Strict as M
import Data.Scientific

data Conv = Conv {scalar :: Scalar, factor :: Integer}
  deriving (Eq, Show)

(>*>) Conv {scalar = a, factor = f} Conv {scalar = b, factor = g} =
  Conv {scalar = a * expScalar b f, factor = f * g}

conversions :: [(Units, Scalar)]
conversions = concatMap explode $ concat [imperialConversions, siConversions, storageConversions, siStorageConversions]
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
    ("ha", ["10000 m^2"]),
    ("lb", ["16 oz", "453.5924 g"]),
    ("st", ["14 lb"]),
    ("qtr", ["28 lb"]),
    ("cwt", ["112 lb"]),
    ("t", ["2240 lb"]),
    ("slug", ["32.17404856 lb"]),
    ("min", ["60 s"]),
    ("hr", ["60 min"]),
    ("day", ["24 hr"]),
    ("bar", ["100000 Pa", "14.50377 psi"]),
    ("hz", ["1 s^-1"])
    --("psi", ["1 lb/in^2"])
  ]

storageConversions =
  [ ("B", ["8 b"])
  ]

siConversion u p x =
  let u' = unitMap ! (p ++ symbol u)
      x' = toRealFloat $ recip x
   in Scalar x' $ Just (fromUnit u')

siConversions = [(fromUnit u, [siConversion u p x]) | u <- metricUnits, (_, p, x) <- siPrefixes]

siStorageConversions = [(fromUnit u, [siConversion u p x]) | u <- storageUnits, (_, p, x) <- siStoragePrefixes]
