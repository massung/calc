{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Calc.Graph where

import Calc.Conv
import Calc.Scalar
import Calc.Units
import Data.Graph.Inductive.Graph hiding (edges)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.List as L
import Data.List.Extra
import Data.Map as M hiding (mapMaybe, (\\))
import Data.Maybe
import Data.Tuple

graph :: Gr Units Scalar
graph = mkGraph nodes $ L.concatMap edges conversions
  where
    nodes = L.map swap $ M.toList nodeMap

nodeMap = M.fromList $ L.zip conversionUnits [1 ..]

edges Conv {to = Scalar _ Nothing} = []
edges Conv {from = from, to = s@(Scalar _ (Just to))} = [(a, b, x), (b, a, recip x)]
  where
    u = from
    a = nodeMap M.! u
    b = nodeMap M.! to
    x = s / fromUnits u

reducedUnits units = mapMaybe match [u | (u, _) <- M.toList nodeMap]
  where
    match node = (node,) <$> simplifyUnits units node

conversionScale from to = do
  a <- M.lookup from nodeMap
  b <- M.lookup to nodeMap
  if a == b
    then Just 1
    else case unLPath $ lesp a b graph of
      [] -> Nothing
      path -> Just $ product [x | (n, x) <- path, n /= a]

convert (Scalar x Nothing) to = Right (Scalar x $ Just to)
convert s@(Scalar x (Just (Units from))) (Units to)
  | from == to = Right s
  | otherwise = case conversion of
    Nothing -> Left "no conversion possible"
    Just scale -> convert (s * scale) (Units to)
  where
    unitsFrom = M.toList from
    unitsTo = M.toList to

    -- units left to be converted
    unconvertedFrom = unitsFrom \\ unitsTo
    unconvertedTo = unitsTo \\ unitsFrom

    -- all possible combinations of reduced units
    xs = L.concatMap (reducedUnits . Units . M.fromList) $ tail $ subsequences unconvertedFrom
    ys = L.concatMap (reducedUnits . Units . M.fromList) $ tail $ subsequences unconvertedTo

    -- limit possible conversions to matching scale factors
    zs = [(x, y, Scalar fx Nothing) | (x, fx) <- xs, (y, fy) <- ys, fx == fy]

    -- find a conversion path in the graph from x -> y
    conversion = firstJust (\(x, y, f) -> (`expScalar` f) <$> conversionScale x y) zs
