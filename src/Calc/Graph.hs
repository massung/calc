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
import Data.Vector as V hiding ((++), mapMaybe)

data Conv = Conv
  { from :: Unit,
    to :: Unit,
    scale :: Double
  }
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    to <- r .: "to"
    scale <- r .: "scale"
    return $ Conv {from = from, to = to, scale = scale}

instance FromField Unit where
  parseField s = pure $ Unit (BS.toString s)

data SIUnits = SIUnits
  { category :: String,
    baseUnit :: String
  }
  deriving(Show)

instance FromNamedRecord SIUnits where
  parseNamedRecord r = do
    category <- r .: "category"
    unit <- r .: "unit"
    return $ SIUnits {category=category, baseUnit=unit}

csv = $(embedStringFile "res/units.csv")
si = $(embedStringFile "res/si.csv")

customConversions = case decodeByName csv of
  Left err -> []
  Right (_, convs) -> V.toList convs

siConversions = case decodeByName si of
  Left err -> []
  Right (_, units) -> L.concatMap (convs . baseUnit) $ V.toList units
    where
      convs u =
        [ conv "z" u 1e-24,
          conv "y" u 1e-21,
          conv "a" u 1e-18,
          conv "f" u 1e-15,
          conv "p" u 1e-12,
          conv "n" u 1e-9,
          conv "u" u 1e-6,
          conv "m" u 1e-3,
          conv "c" u 1e-2,
          conv "d" u 1e-1,
          conv "da" u 10,
          conv "h" u 1e2,
          conv "k" u 1e3,
          conv "M" u 1e6,
          conv "G" u 1e9,
          conv "T" u 1e12,
          conv "P" u 1e15,
          conv "E" u 1e18,
          conv "Z" u 1e21,
          conv "Y" u 1e24
        ]

      -- apply si prefix to base units with scale factor
      conv prefix u scale =
        Conv {
          from=Unit $ prefix ++ u,
          to=Unit u,
          scale=scale
        }

conversions = siConversions ++ customConversions

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
    x = Scalar (scale conv) (Just $ U.fromList [(from conv, -1), (to conv, 1)])

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
              | True -> Just $ expScalar x (Scalar toE Nothing)
