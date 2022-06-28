{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Conv where

import Calc.Scalar
import Calc.Units
import Calc.Units.Base
import Data.ByteString.UTF8 as BS
import Data.Csv as Csv
import Data.Either
import Data.FileEmbed
import Data.List as L
import Data.Vector as V hiding ((++))

data Conv = Conv {from :: Unit, to :: Scalar}
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    to <- r .: "to"
    return Conv {from = from, to = to}

convCsv = $(embedStringFile "res/conv.csv")

conversions = base ++ siConversions
  where
    base = fromRight [] $ V.toList . snd <$> decodeByName convCsv

siConversions = L.concat [L.map (convs u) si | (u, si) <- siUnits]
  where
    convs base (unit, n) =
      Conv
        { from = base,
          to = Scalar (recip n) (Just $ singletonUnits unit)
        }

conversionUnits = nub $ L.concatMap units conversions
  where
    units Conv {from=from, to=Scalar _ Nothing} = [singletonUnits from]
    units Conv {from=from, to=Scalar _ (Just x)} = [singletonUnits from, x]
