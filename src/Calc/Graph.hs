{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Graph where

import Calc.Scalar
import Calc.Units as U
import Data.ByteString.UTF8 as BS
import Data.Csv
import Data.FileEmbed
import Data.Graph.Inductive.Graph hiding (edges)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.List as L
import Data.Map as M hiding (mapMaybe)
import Data.Maybe
import Data.Vector as V hiding (mapMaybe)

data Conv = Conv
  { from :: Unit,
    to :: Unit,
    scale :: Scalar
  }
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    to <- r .: "to"
    scale <- r .: "scale"
    let n = Scalar scale (Just $ U.fromList [(from, -1), (to, 1)])
     in return $ Conv {from = from, to = to, scale = n}

instance FromField Unit where
  parseField s = pure $ Unit (BS.toString s)

csv = $(embedStringFile "res/units.csv")

conversions = case decodeByName csv of
  Left err -> []
  Right (_, convs) -> V.toList convs

distinctUnits = nub $ names conversions
  where
    names [] = []
    names (conv : convs) = from conv : to conv : names convs

unitsMap = M.fromList $ L.zip distinctUnits [1 ..]

edges [] = []
edges (conv : convs) = (a, b, x) : (b, a, recip x) : edges convs
  where
    a = unitsMap M.! from conv
    b = unitsMap M.! to conv
    x = scale conv

graph :: Gr Unit Scalar
graph = mkGraph nodes $ edges conversions
  where
    nodes = [(v, k) | (k, v) <- M.toList unitsMap]

conversionScale from to = do
  a <- M.lookup from unitsMap
  b <- M.lookup to unitsMap
  if a == b
    then Just 1
    else case unLPath $ lesp a b graph of
      [] -> Nothing
      path -> Just $ L.foldl (*) 1.0 $ L.map snd $ L.filter ((/= a) . fst) path

convertScalar :: Scalar -> Units -> Either String Scalar
convertScalar (Scalar x Nothing) to = Right $ Scalar x $ Just to
convertScalar s@(Scalar x (Just (Units from))) p@(Units to) =
  if from == to
    then Right s
    else case listToMaybe $ mapMaybe (matchConversion unconvertedFrom) unconvertedTo of
      Nothing -> Left "no conversion possible"
      Just scale -> convertScalar (s * scale) p
  where
    unitsFrom = M.toList from
    unitsTo = M.toList to

    -- the next unit that needs to be converted and left to convert to
    unconvertedFrom = L.head $ unitsFrom L.\\ unitsTo
    unconvertedTo = unitsTo L.\\ unitsFrom

    -- see if there's a valid match from
    matchConversion (fromU, fromE) (toU, toE) =
      case conversionScale fromU toU of
        Nothing -> Nothing
        Just x ->
          if
              | fromE /= toE -> Nothing
              | fromE < 0 -> Just $ recip x
              | True -> Just x
