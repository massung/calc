{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Calc.Graph where

import Calc.Conv
import Calc.Dim
import Calc.Scalar
import Calc.Units
import Data.Graph.Inductive.Graph hiding (edges)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.List
import Data.Map.Strict as M hiding (mapMaybe, (\\))
import Data.Tuple

dimsGraph :: Gr Dims Conv
dimsGraph = mkGraph nodes $ concatMap nodeEdges edges
  where
    nodes = [swap d | d <- toList dimsNodeMap]

    -- conversions between different dimensions
    edges = [(from, to, conv) | conv@(Conv from (Scalar _ (Just to))) <- conversions, from /= to]

    -- lookup nodes for conversion pairs
    nodeEdges (from, to, conv) =
      [ (fromNode, toNode, conv),
        (toNode, fromNode, recipConv conv)
      ]
      where
        fromNode = dimsNodeMap ! unitsDims from
        toNode = dimsNodeMap ! unitsDims to

unitsGraph :: Gr Units Scalar
unitsGraph = mkGraph nodes $ concatMap nodeEdges edges
  where
    nodes = [swap d | d <- toList unitsNodeMap]

    edges = [(from, to, x) | Conv from x@(Scalar _ (Just to)) <- conversions ++ derivedConvs]

    nodeEdges (from, to, x) =
      let fromNode = unitsNodeMap ! from
          toNode = unitsNodeMap ! to
       in [ (fromNode, toNode, x),
            (toNode, fromNode, recip x)
          ]

dimsNodeMap = fromList $ zip conversionDims [1 ..]

unitsNodeMap = fromList $ zip (nub $ baseUnits ++ conversionUnits) [1 ..]

convertDims :: Units -> Units -> Maybe [Conv]
convertDims from to =
  let fromDims = unitsDims from
      toDims = unitsDims to
   in if fromDims == toDims
        then Just []
        else do
          fromNode <- M.lookup fromDims dimsNodeMap
          toNode <- M.lookup toDims dimsNodeMap

          -- return the chain of dim conversions needed
          case unLPath $ lesp fromNode toNode dimsGraph of
            [] -> Nothing
            xs -> Just [x | (n, x) <- xs, n /= fromNode]

convertUnits :: Units -> Units -> Maybe Scalar
convertUnits (Units from) (Units to) =
  if from == to
    then Just 1
    else do
      fromNode <- M.lookup (Units from') unitsNodeMap
      toNode <- M.lookup (Units to') unitsNodeMap

      -- find the path connecting units
      case unLPath $ lesp fromNode toNode unitsGraph of
        [] -> Nothing
        xs -> Just $ product [x | (n, x) <- xs, n /= fromNode]
  where
    (from', factor) = simplify from
    to' = simplifyBy factor to

--   | from == to = Nothing
--   | fromDims == toDims = convPath
--   | otherwise =
--   where
--     fromDims = unitsDims from
--     toDims = unitsDims to

-- conversionScale from to = do
--   a <- M.lookup from nodeMap
--   b <- M.lookup to nodeMap
--   if a == b
--     then Just 1
--     else case unLPath $ lesp a b graph of
--       [] -> Nothing
--       path -> Just $ product [x | (n, x) <- path, n /= a]

-- convert :: Scalar -> Units -> Either String Scalar
-- convert (Scalar x Nothing) to = Right (Scalar x $ Just to)
-- convert s@(Scalar x (Just (Units from))) (Units to)
--   | from == to = Right s
--   | otherwise = case conversion of
--     Nothing -> Left "no conversion possible"
--     Just scale -> convert (s * scale) (Units to)
--   where
--     unitsFrom = M.toList from
--     unitsTo = M.toList to

--     -- units left to be converted
--     unconvertedFrom = unitsFrom \\ unitsTo
--     unconvertedTo = unitsTo \\ unitsFrom

--     -- all possible combinations of reduced units
--     xs = L.concatMap (reducedUnits . Units . M.fromList) $ tail $ subsequences unconvertedFrom
--     ys = L.concatMap (reducedUnits . Units . M.fromList) $ tail $ subsequences unconvertedTo

--     -- limit possible conversions to matching scale factors
--     zs = [(x, y, Scalar fx Nothing) | (x, fx) <- xs, (y, fy) <- ys, fx == fy]

--     -- find a conversion path in the graph from x -> y
--     conversion = firstJust (\(x, y, f) -> (`expScalar` f) <$> conversionScale x y) zs
