{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where

import Calc.Parser.Scalar
import Calc.SI
import Calc.Scalar
import Calc.Units
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M hiding (member)
import Data.Maybe
import Data.Set as S

convMap :: Map Units [(Units, Scalar)]
convMap = M.fromListWith (++) (conversions ++ recips)
  where
    recips = concat [recipConv conv | conv <- conversions]

conversions :: [(Units, [(Units, Scalar)])]
conversions =  [conv from xs | (from, xs) <- convs]
  where
    convs =
      concat
        [ imperialConvs,
          metricConvs,
          storageConvs,
          siStorageConvs
        ]

conv :: Units -> [Scalar] -> (Units, [(Units, Scalar)])
conv from xs = (from, [(u, to / from') | to@(Scalar _ u) <- xs])
  where
    from' = fromUnits from

recipConv :: (Units, [(Units, Scalar)]) -> [(Units, [(Units, Scalar)])]
recipConv (from, xs) = [(to, [(from, recip x)]) | (to, x) <- xs]

imperialConvs =
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
  ]

storageConvs =
  [ ("B", ["8 b"])
  ]

siConv u p x =
  let u' = unitMap ! (p ++ symbol u)
      x' = toRational $ recip x
   in Scalar x' $ fromUnit u'

siStorageConvs = [(fromUnit u, [siConv u p x]) | u <- storageUnits, (_, p, x) <- siPrefixes, x > 1]

metricConvs = [(fromUnit u, [siConv u p x]) | u <- metricUnits, (_, p, x) <- siPrefixes]

convert a b = Right 1

convertX x@(Scalar _ from) to =
  msum [dfs (x * expScalar x' e) (S.singleton from') to | (u, x') <- convs from']
  where
    (from', e) = simplifyUnits from

dfs x@(Scalar _ from) ex to =
  if from == to
    then Just x
    else
      let (from', e) = simplifyUnits from
       in msum [dfs (x * expScalar x' e) (S.insert u ex) to | (u, x') <- paths from']
  where
    paths = L.filter ((`S.notMember` ex) . fst) . convs

convs u = fromMaybe [] $ convMap !? u

{-
bfs :: [Scalar] -> [(Scalar, Integer)] -> Set Units -> Units -> Maybe [Scalar]
bfs path [] _ _ = Nothing
bfs path ((Scalar _ Nothing, factor):xs) ex to = Nothing
bfs path ((x@(Scalar _ (Just u)), factor):xs) ex to
  | u == to = Just (expScalar x factor:path)
  | member u ex = bfs path xs ex to
  | otherwise = bfs (x:path) (xs ++ next) (insert u ex) to
  where
    next = [expScalar conv factor | conv <- fromMaybe [] $ M.lookup u convMap]
-}

