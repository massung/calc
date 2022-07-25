{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where
{-
import Calc.Dims
import Calc.Error
import Calc.Parser.Scalar
import Calc.Scalar
import Calc.Units
import Control.Applicative
import Data.Either.Extra
import Data.Foldable as F
import Data.List as L hiding (mapMaybe)
import Data.Map.Strict as M hiding (mapMaybe, member)
import Data.Maybe
import Data.Set as S

convMap :: Map Units [(Units, Scalar)]
convMap = M.fromListWith (++) (conversions ++ recips)
  where
    recips = concat [recipConv conv | conv <- conversions]

conversions :: [(Units, [(Units, Scalar)])]
conversions = [conv from xs | (from, xs) <- convs]
  where
    convs =
      concat
        [ lengthConvs,
          areaConvs,
          volumeConvs,
          massConvs,
          timeConvs,
          speedConvs,
          pressureConvs,
          forceConvs,
          energyConvs,
          powerConvs,
          angularConvs,
          electricalConvs,
          metricConvs,
          storageConvs,
          siStorageConvs,
          derivedConvs
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

lengthConvs =
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
    ("rod", ["25 link"])
  ]

areaConvs =
  [ ("acre", ["43560.04 ft^2"]),
    ("ha", ["10000 m^2", "2.471052 acre"])
  ]

volumeConvs =
  [ ("tbsp", ["3 tsp"]),
    ("floz", ["2 tbsp"]),
    ("c", ["8 floz"]),
    ("pt", ["2 c"]),
    ("qt", ["2 pt"]),
    ("gal", ["4 qt"]),
    ("L", ["1000 cm^3", "35.19508 floz"])
  ]

massConvs =
  [ ("lb", ["16 oz", "453.5924 g"]),
    ("st", ["14 lb"]),
    ("qtr", ["28 lb"]),
    ("cwt", ["112 lb"]),
    ("t", ["2240 lb"]),
    ("slug", ["32.17404856 lb"])
  ]

timeConvs =
  [ ("min", ["60 s"]),
    ("hr", ["60 min"]),
    ("day", ["24 hr"]),
    ("hz", ["1 s^-1"])
  ]

speedConvs =
  [ ("kn", ["1.150779 mi/hr"]),
    ("mph", ["1 mi/hr"])
  ]

pressureConvs =
  [ ("Pa", ["1 N/m^2"]),
    ("bar", ["100000 Pa", "14.50377 psi"]),
    ("psi", ["1 lb/in^2"])
  ]

forceConvs =
  [ ("N", ["1 kg m/s^2", "0.224809 lbf"])
  ]

energyConvs =
  [ ("J", ["1 kg m^2/s^2", "1 N m", "1 Pa m^3"]),
    ("BTU", ["1055.056 J"])
  ]

powerConvs =
  [ ("hp", ["745.7 W"])
  ]

angularConvs =
  [ ("rad", ["57.29578 deg"]),
    ("rev", ["180 deg"]),
    ("arcs", ["2.78e-4 deg"]),
    ("arcm", ["0.016666668 deg"])
  ]

electricalConvs =
  [ ("C", ["1 A s"]),
    ("V", ["1 J/C", "1 kg m^2/A^-1 s^-3"]),
    ("O", ["1 V/A", "1 kg m^2/A^-2 s^-3"]),
    ("W", ["1 J/s", "1 V A", "1 kg m^2/s^-3"]),
    ("F", ["1 C/V", "1 s^4 A^2/kg m^2"])
  ]

storageConvs =
  [ ("B", ["8 b"])
  ]

derivedConvs =
  [ ("V C", ["1 J"]),
    ("A s", ["1 C"]),
    ("O A", ["1 V"]),
    ("W s", ["1 J"]),
    ("W/A", ["1 V"]),
    ("F V", ["1 C"])
  ]

siConv u p x =
  let u' = unitMap ! (p ++ symbol u)
      x' = toRational $ recip x
   in Scalar x' $ fromUnit u'

metricConvs = [(fromUnit u, [siConv u p x]) | u <- metricUnits, (_, p, x) <- siPrefixes]

siStorageConvs = [(fromUnit u, [siConv u p x]) | u <- storageUnits, (_, p, x) <- storagePrefixes]

harmonize x@(Scalar _ from) to =
  maybe (Left $ ConversionError from to) (Right . (x*)) (harmonizeDims from to)

harmonizeDims :: Units -> Units -> Maybe Scalar
harmonizeDims from to =
  let from' = dimsUnits from
      to' = dimsUnits to
   in M.foldl' mappend (Just 1) $ M.intersectionWith conv from' to'
  where
    conv (from, x) (to, _) = (^^x) <$> convertUnits (fromUnit from) (fromUnit to)

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
-}