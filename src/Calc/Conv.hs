{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where

import Calc.Dim
import Calc.Error
import Calc.Parser.Scalar
import Calc.SI
import Calc.Scalar
import Calc.Units
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

dimsConvMap :: Map Units [(Dims, Scalar)]
dimsConvMap = M.mapWithKey convDims convMap
  where
    convDims from convs =
      let d = dims from in L.filter ((/= d) . fst) [first dims x | x <- convs]

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
    ("ha", ["10000 m^2"]),
    ("tbsp", ["3 tsp"]),
    ("floz", ["1.6 tbsp"]),
    ("gill", ["5 floz"]),
    ("cup", ["8 floz"]),
    ("pt", ["20 cup"]),
    ("qt", ["40 pt"]),
    ("gal", ["160 floz"]),
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
    ("J", ["2.78 W/hr"]),
    ("BTU", ["1055.056 J"]),
    ("N", ["1 kg m/s^2", "0.224809 lbf"]),
    ("knot", ["1.150779 mi/hr"]),
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

convert x@(Scalar f from) to =
  if nullUnits from
    then Right $ Scalar f to
    else let x' = conv . (x*) =<< convertDims from to
          in maybeToEither (ConversionError from to) x'
  where
    conv x@(Scalar _ from) = (x*) <$> convertUnits from to

convertDims from to =
  if nullUnits to || dims from == dims to
    then Just 1
    else listToMaybe [x | (d, x) <- convs, d == dims to]
  where
    convs = M.findWithDefault [] from dimsConvMap

convertUnits from to =
  if nullUnits to || from == to
    then Just 1
    else case product <$> msum convs of
      Nothing -> Nothing
      Just conv -> (conv *) <$> convertUnits (from <> scalarUnits conv) to
  where
    xs = L.map fromUnitList $ tail $ subsequences $ M.toList $ unconvertedUnits from to
    ys = L.map fromUnitList $ tail $ subsequences $ M.toList $ unconvertedUnits to from

    -- all possible conversions that could happen
    convs = [dfs [] from' to' (S.singleton from') | from' <- xs, to' <- ys]

dfs xs from to ex =
  if from == to
    then Just xs
    else msum [dfs (x : xs) u to (S.insert u ex) | (u, x) <- unitsConvs from, S.notMember u ex]

unitsConvs from =
  if exp == 1
    then convs from
    else convs from ++ [(mapUnits (* exp) u, x ^^ exp) | (u, x) <- convs from']
  where
    convs = fromMaybe [] . (convMap !?)

    -- simplified units
    (from', exp) = simplifyUnits from

unconvertedUnits (Units a) (Units b) = (M.\\) a b
