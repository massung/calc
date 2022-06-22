{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Graph where

import Data.ByteString.UTF8 as BS
import Data.Csv
import Data.Either
import Data.FileEmbed
import Data.Graph.Inductive.Graph hiding (edges)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.List as L
import Data.Map as M
import Data.Scientific
import Data.Vector as V

-- units that can be multiplied and divided
data Units = Units
  { category :: [String],
    abbrev :: [String]
  }

instance Show Units where
  show = L.concat . abbrev

instance Eq Units where
  (==) a b = category a == category b && abbrev a == abbrev b

instance Ord Units where
  (<=) a b = category a <= category b && abbrev a <= abbrev b

-- csv conversion record
data Conv = Conv
  { from :: Units,
    to :: Units,
    scale :: Scientific
  }
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r =
    Conv
      <$> r .: "category"
      <*> r .: "from"
      <*> r .: "to"
      <*> r .: "scale"

-- compile the conversion table into the executable
csv = $(embedStringFile "res/units.csv")

-- parse all the conversions from res/units.csv
conversions = case decodeByName csv of
  Left err -> []
  Right (_, convs) -> V.toList convs

-- list of all unit names
units = nub $ names conversions
  where
    names [] = []
    names (conv : convs) = from conv : to conv : names convs

-- mapping of unit names to graph node index
unitsMap = M.fromList $ L.zip units [1 ..]

-- labelled graph edges using scale as label
edges [] = []
edges (conv : convs) = (a, b, x) : (b, a, recip x) : edges convs
  where
    a = unitsMap M.! from conv
    b = unitsMap M.! to conv
    x = scale conv

-- graph containing all units and conversions
graph :: Gr String Scientific
graph = mkGraph [(v, k) | (k, v) <- M.toList unitsMap] $ edges conversions

-- attempt to convert from one unit type to another
conversionScale from to = do
  a <- M.lookup from unitsMap
  b <- M.lookup to unitsMap
  if a == b
    then Just 1
    else case unLPath $ lesp a b graph of
      [] -> Nothing
      path -> Just $ L.foldl (*) 1.0 $ L.map snd $ L.tail $ L.reverse path
