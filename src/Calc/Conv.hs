{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where

import Calc.Dim
import Calc.Error
import Calc.Parser.Scalar
import Calc.SI
import Calc.Scalar
import Calc.Units
import Control.Applicative
import Data.Either.Extra
import Data.Foldable as F
import Data.List as L hiding (mapMaybe)
import Data.Map.Strict as M hiding (mapMaybe, member)
import Data.Maybe
import Data.Set as S
import Data.Tuple.Extra

convMap :: Map Units [(Units, Scalar)]
convMap = M.fromListWith (++) (conversions ++ recips)
  where
    recips = concat [recipConv conv | conv <- conversions]

conversions :: [(Units, [(Units, Scalar)])]
conversions = [conv from xs | (from, xs) <- convs]
  where
    convs =
      concat
        [ imperialConvs,
          metricConvs,
          storageConvs,
          siStorageConvs
        ]

conv :: Units -> [Scalar] -> (Units, [(Units, Scalar)])
conv from xs = (from, [(u, to / fromUnits from) | to@(Scalar _ u) <- xs])

recipConv :: (Units, [(Units, Scalar)]) -> [(Units, [(Units, Scalar)])]
recipConv (from, xs) = [(to, [(from, recip x)]) | (to, x) <- xs]

dimsConvMap :: Map (Dims, Dims) (Map Units Scalar)
dimsConvMap = M.foldlWithKey' convDims M.empty convMap
  where
    convDims m from convs = L.foldl' (insConv from) m convs

    -- insert a single conversion
    insConv from m conv@(u, x) =
      if dims u == dims from
        then m
        else
          let k = (dims from, dims u)
           in M.alter (\m -> (M.insert from x <$> m) <|> Just (M.singleton from x)) k m

imperialConvs =
  [ ("in", ["1000 mil"]),
    ("h", ["4 in"]),
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
    ("ha", ["10000 m^2", "2.471052 acre"]),
    ("tbsp", ["3 tsp"]),
    ("floz", ["2 tbsp"]),
    ("c", ["8 floz"]),
    ("pt", ["2 c"]),
    ("qt", ["2 pt"]),
    ("gal", ["4 qt"]),
    ("L", ["1000 cm^3", "35.19508 floz"]),
    ("lb", ["16 oz", "453.5924 g"]),
    ("st", ["14 lb"]),
    ("qtr", ["28 lb"]),
    ("cwt", ["112 lb"]),
    ("t", ["2240 lb"]),
    ("slug", ["32.17404856 lb"]),
    ("min", ["60 s"]),
    ("hr", ["60 min"]),
    ("day", ["24 hr"]),
    ("Pa", ["1 N/m^2"]),
    ("bar", ["100000 Pa", "14.50377 psi"]),
    ("psi", ["1 lb/in^2"]),
    ("hz", ["1 s^-1"]),
    ("HP", ["745.7 W"]),
    ("J", ["1 kg m^2/s^2", "1 N m", "1 Pa m^3"]),
    ("BTU", ["1055.056 J"]),
    ("N", ["1 kg m/s^2", "0.224809 lbf"]),
    ("kn", ["1.150779 mi/hr"]),
    ("rad", ["57.29578 deg"]),
    ("rev", ["180 deg"]),
    ("arcs", ["2.78e-4 deg"]),
    ("arcm", ["0.016666668 deg"]),
    ("V", ["1 J/C"]),
    ("A", ["1 C/s"]),
    ("O", ["1 V/A"]),
    ("W", ["1 J/s", "1 V A"]),
    ("F", ["1 C/V"])
  ]

storageConvs = [("B", ["8 b"])]

siConv u p x =
  let u' = unitMap ! (p ++ symbol u)
      x' = toRational $ recip x
   in Scalar x' $ fromUnit u'

metricConvs = [(fromUnit u, [siConv u p x]) | u <- metricUnits, (_, p, x) <- siPrefixes]

siStorageConvs = [(fromUnit u, [siConv u p x]) | u <- storageUnits, (_, p, x) <- storagePrefixes]

harmonize x@(Scalar f from) to =
  if nullUnits from
    then Right $ Scalar f to
    else
      let similar = M.intersectionWith (,) (dims' from) (dims' to)
       in M.foldl' conv (Right x) similar
  where
    conv (Right x) (from', to') = case convertUnits from' to' of
      Nothing -> Left $ ConversionError from' to'
      Just y -> Right $ x * y
    conv err _ = err

{-
Conversion algorithm:

  1. Check to see if the dimensionality of the units are the same (e.g. ft/s -> m/hr)
    a. No? Goto 2
    b. Yes? Attempt to convert 1:1 each unit pair (e.g. ft->m and s->hr)
  2. Search for a conversion between dimensionalities (e.g. length^2 -> area)
    a. No? Fail conversion
    b. Yes? Apply conversion and goto 1.b.

Searching for a conversion between dimensionalities:

  1. Is there a set of possible conversions? (e.g. length^2 -> area)
    a. No? Fail conversion
    b. Yes? Is there an exact conversion? (e.g. ft^2)
      i. No? Goto 2
      ii. Yes? Return conversion
  2. Is there a unit with a conversion we can convert to? (e.g. in^2 -> ft^2)
    a. No? Fail conversion
    b. Yes? Return conversion * dimensionality conversion
-}

convert x@(Scalar f from) to =
  if nullUnits from
    then Right $ Scalar f to
    else
      let x' = conv . (x *) =<< convertDims from to
       in maybeToEither (ConversionError from to) x'
  where
    conv x@(Scalar _ from) = (x *) <$> convertUnits from to

convertDims from to =
  if nullUnits to || dims from == dims to
    then Just 1
    else do
      convs <- M.lookup (dims from, dims to) dimsConvMap
      M.lookup from convs
        <|> msum [(x *) <$> convertUnits from u | (u, x) <- M.toList convs]

convertUnits from to =
  if nullUnits to || from == to
    then Just 1
    else product <$> mconcat convs
  where
    from' = unconvertedUnits from to
    to' = unconvertedUnits to from

    -- units conversions between matching dimensions
    convs = [dfs [] x y (S.singleton x) | x <- from', y <- to', dims x == dims y]

dfs xs from to ex =
  if from == to
    then Just xs
    else msum [dfs (x : xs) u to (S.insert u ex) | (u, x) <- unitsConvs from, S.notMember u ex]

unitsConvs from = [(mapUnits (* exp) u, x ^^ exp) | (u, x) <- fromMaybe [] $ convMap !? from']
  where
    (from', exp) = simplifyUnits from

unconvertedUnits (Units a) (Units b) = [Units u | u <- M.splitRoot $ (M.\\) a b, not (M.null u)]
