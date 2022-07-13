{-# LANGUAGE OverloadedStrings #-}

module Calc.Graph where
{-
import Calc.Conv
import Calc.Scalar
import Calc.Units
import Data.Foldable as F
import Data.Graph.Inductive.Graph hiding (edges, nodes)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.List.Extra
import Data.Map.Strict as M hiding (mapMaybe, (\\))
import Data.Tuple

graph :: Gr Units Scalar
graph = mkGraph nodes edges
  where
    nodes = [swap d | d <- M.toList nodeMap]

nodeMap = M.fromList $ zip units [1 ..]

edges = concat [mkEdges from x to | (from, Scalar x (Just to)) <- conversions]
  where
    mkEdges from x to =
      let fromNode = nodeMap ! from
          toNode = nodeMap ! to
       in [ (fromNode, toNode, Scalar x (Just to) / fromUnits from),
            (toNode, fromNode, Scalar (recip x) (Just from) / fromUnits to)
          ]

conversionPath from to = do
  fromNode <- M.lookup from nodeMap
  toNode <- M.lookup to nodeMap
  case unLPath (lesp fromNode toNode graph) of
    [] -> Nothing
    xs -> Just [x | (node, x) <- xs, node /= fromNode]

conversionScale from to = do
  product <$> conversionPath from to

convertUnits :: Units -> Units -> Maybe Scalar
convertUnits (Units from) (Units to) = msum convs
  where
    unitsFrom = M.toList from
    unitsTo = M.toList to

    -- units left to be converted
    unconvertedFrom = unitsFrom \\ unitsTo
    unconvertedTo = unitsTo \\ unitsFrom

    -- all possible combinations of units
    xs = [simplify $ fromList x | x <- tail $ subsequences unconvertedFrom]
    ys = [simplify $ fromList y | y <- tail $ subsequences unconvertedTo]

    -- try to convert between unit combinations
    convs = [conversionScale (Units from) (Units to) | (from, x) <- xs, (to, y) <- ys]

convert :: Scalar -> Units -> Either String Scalar
convert (Scalar x Nothing) to = Right $ Scalar x (Just to)
convert x@(Scalar _ (Just from)) to
  | from == to = Right x
  | otherwise = case convertUnits from to of
    Nothing -> Left "no convert"
    Just y -> convert (x * y) to
-}
