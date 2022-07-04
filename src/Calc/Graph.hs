{-# LANGUAGE OverloadedStrings #-}

module Calc.Graph where

import Calc.Conv
import Calc.Dim
import Calc.Scalar
import Calc.Units
import Data.Graph.Inductive.Graph hiding (edges)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.List as L
import Data.List.Extra
import Data.Map.Strict as M hiding (mapMaybe, (\\))
import Data.Maybe
import Data.Tuple

unitsGraph :: Gr Units Scalar
unitsGraph = mkGraph nodes $ mapMaybe edge conversions
  where
    nodes = [swap d | d <- toList unitsNodeMap]
    edge (Conv from to x) = do
      from' <- M.lookup from unitsNodeMap
      to' <- M.lookup to unitsNodeMap
      return (from', to', x)

unitsNodeMap = fromList $ zip (nub $ units ++ conversionUnits) [1 ..]

conversionScale from to = do
  a <- M.lookup from unitsNodeMap
  b <- M.lookup to unitsNodeMap
  if a == b
    then Just 1
    else case unLPath $ lesp a b unitsGraph of
      [] -> Nothing
      xs -> Just $ product [x | (n, x) <- xs, n /= a]

convertUnits :: Units -> Units -> Maybe Scalar
convertUnits (Units from) (Units to) = firstJust conversion conversions
  where
    unitsFrom = M.toList from
    unitsTo = M.toList to

    -- units left to be converted
    unconvertedFrom = unitsFrom \\ unitsTo
    unconvertedTo = unitsTo \\ unitsFrom

    -- all possible combinations of units
    xs = [simplify $ fromList x | x <- tail $ subsequences unconvertedFrom]
    ys = [simplify $ fromList y | y <- tail $ subsequences unconvertedTo]

    -- possible matching conversions with matching factors
    conversions = [(Units x, Units y, fx) | (x, fx) <- xs, (y, fy) <- ys, fx == fy]

    -- search graph for conversion scale
    conversion (x, y, f) = (`expScalar` f) <$> conversionScale x y

convert :: Scalar -> Units -> Either String Scalar
convert (Scalar x Nothing) to = Right $ Scalar x (Just to)
convert x@(Scalar _ (Just from)) to
  | from == to = Right x
  | otherwise = case convertUnits from to of
    Nothing -> Left "no convert"
    Just y -> convert (x * y) to
