{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Conv where

import Calc.Scalar
import Calc.Units
import Data.ByteString.UTF8 as BS
import Data.Csv as Csv
import Data.Either
import Data.FileEmbed
import Data.Map as M
import Data.Vector as V
import Text.Parsec

data Conv = Conv {from :: Units, to :: Scalar}
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    to <- r .: "to"
    scale <- r .: "scale"
    return Conv {from = from, to = to, scale = scale}

convCsv = $(embedStringFile "res/conv.csv")

conversions :: [Conv]
conversions = fromRight [] $ V.toList . snd <$> decodeByName convCsv

conversionScalar conv = Scalar (scale conv) (Just units)
  where
    units = multiplyUnits (to conv) (recipUnits $ from conv)
