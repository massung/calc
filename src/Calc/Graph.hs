{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Graph where

import Calc.Scalar
import Calc.Units as U
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

data Conv =
  Conv
    { from :: Unit,
      to:: Unit,
      scale :: Scalar
    }
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    to <- r .: "to"
    scale <- r .: "scale"
    let n = U.fromList [(from,1), (to, scale)]
      in return $ Conv { from=from, to=to, scale=Scalar 1 (Just n)}

instance FromField Unit where
  parseField s = pure $ Unit (BS.toString s)

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
graph :: Gr Unit Scalar
graph = mkGraph nodes $ edges conversions
  where
    nodes = [(v, k) | (k, v) <- M.toList unitsMap]

-- attempt to convert from one unit type to another
conversionScale from to = do
  a <- M.lookup from unitsMap
  b <- M.lookup to unitsMap
  if a == b
    then Just 1
    else case unLPath $ lesp a b graph of
      [] -> Nothing
      path -> Just $ L.foldl (*) 1.0 $ L.map snd $ L.tail $ L.reverse path
