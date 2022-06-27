{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Graph where

import Calc.Conv
import Calc.Scalar
import Calc.Units
import Data.Graph.Inductive.Graph hiding (edges)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.List as L
import Data.Map as M hiding (mapMaybe)
import Data.Tuple

graph :: Gr Units Scalar
graph = mkGraph nodes $ L.concatMap edges conversions
  where
    nodes = L.map swap $ M.toList nodeMap

nodeMap = M.fromList $ L.zip units [1 ..]
  where
    units = [singletonUnits u | (_, u) <- M.toList unitsMap]

edges Conv {to = Scalar _ Nothing} = []
edges Conv {from = from, to = s@(Scalar _ (Just to))} = [(a, b, x), (b, a, recip x)]
  where
    u = singletonUnits from
    a = nodeMap M.! u
    b = nodeMap M.! to
    x = s / fromUnits u

conversionScale from to = do
  a <- M.lookup from nodeMap
  b <- M.lookup to nodeMap
  if a == b
    then Just 1
    else case unLPath $ lesp a b graph of
      [] -> Nothing
      path -> Just $ L.foldl (*) 1.0 $ L.map snd $ L.filter ((/= a) . fst) path

-- convert (Scalar x Nothing) = Right . Scalar x . Just
-- convert s@(Scalar x (Just from)) to =
--   if from == to
--     then Right s
--     else case

-- convertScalar (Scalar x Nothing) to = Right $ Scalar x (Just to)
-- convertScalar s@(Scalar x (Just (Units from))) p@(Units to) =
--   if from == to
--     then Right s
--     else case listToMaybe $ mapMaybe (matchConversion unconvertedFrom) unconvertedTo of
--       Nothing -> Left "no conversion possible"
--       Just scale -> convertScalar (s * scale) p
--   where
--     unitsFrom = M.toList from
--     unitsTo = M.toList to

--     -- the next unit that needs to be converted and left to convert to
--     unconvertedFrom = L.head $ unitsFrom L.\\ unitsTo
--     unconvertedTo = unitsTo L.\\ unitsFrom

--     -- see if there's a valid match from
--     matchConversion (fromU, fromE) (toU, toE) =
--       case conversionScale fromU toU of
--         Nothing -> Nothing
--         Just x ->
--           if
--               | fromE /= toE -> Nothing
--               | fromE < 0 -> Just $ recip x
--               | True -> Just $ expScalar x (Scalar toE Nothing)
