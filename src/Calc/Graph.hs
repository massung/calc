{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Calc.Graph where

import Calc.Conv
import Calc.Scalar
import Calc.Units
import Data.Graph.Inductive.Graph hiding (edges, nodes)
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

nodes = [u | (u, _) <- M.toList nodeMap]

nodeMap = M.fromList $ L.zip conversionUnits [1 ..]

edges Conv {to = Scalar _ Nothing} = []
edges Conv {from = from, to = s@(Scalar _ (Just to))} = [(a, b, x), (b, a, recip x)]
  where
    u = singletonUnits from
    a = nodeMap M.! u
    b = nodeMap M.! to
    x = s / fromUnits u

simplifiedUnits units = mapMaybe match nodes
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
    xs = [Units $ M.fromList u | u <- tail $ subsequences $ M.toList from]
    ys = [Units $ M.fromList u | u <- tail $ subsequences $ M.toList to]

    -- find the first path in the graph from -> to
    conversion = firstJust (uncurry conversionScale) [(x, y) | x <- xs, y <- ys]
