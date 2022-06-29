{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Conv where

import Calc.Scalar
import Calc.Units
import Calc.Units.Base
import Calc.Units.Dim
import Data.ByteString.UTF8 as BS
import Data.Csv as Csv
import Data.Either
import Data.FileEmbed
import Data.List as L
import Data.Vector as V hiding ((++))

data Conv = Conv {from :: Units, to :: Scalar}
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    to <- r .: "to"
    return Conv {from = from, to = to}

convCsv = $(embedStringFile "res/conv.csv")

conversions = let all = base ++ siConversions in all ++ expConversions all
  where
    base = fromRight [] $ V.toList . snd <$> decodeByName convCsv

siConversions = L.concat [L.map (convs $ singletonUnits u) si | (u, si) <- siUnits]
  where
    convs base (unit, n) =
      Conv {from = base, to = Scalar (recip n) (Just $ singletonUnits unit)}

expConversions [] = []
expConversions (Conv {from = from, to = to} : convs) = L.map expConv [2 .. 3] ++ expConversions convs
  where
    expConv e = Conv {from = expUnits from e, to = expScalar to (Scalar e Nothing)}

conversionUnits = nub $ L.concatMap units conversions
  where
    units Conv {from = from, to = Scalar _ Nothing} = [from]
    units Conv {from = from, to = Scalar _ (Just x)} = [from, x]
