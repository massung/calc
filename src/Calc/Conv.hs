{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Conv where

import Calc.Parser.Scalar
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units
import Data.Csv as Csv
import Data.Either
import Data.FileEmbed
import Data.Foldable as F
import Data.List as L
import Data.Maybe

data Conv = Conv {from :: Units, to :: Units, scale :: Scalar}
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    scale <- r .: "to"
    return $ Conv from (fromJust $ scalarUnits scale) (scale / fromUnits from)

baseConversions :: [Conv]
baseConversions = fromRight (error "ack!") $ F.toList . snd <$> decodeByName csv
  where
    csv = $(embedStringFile "units/conv.csv")

siConversions = [siConv to p | to <- siUnits, p <- siPrefixes]
  where
    siConv to (p, x) = Conv from (fromUnit to 1) scale
      where
        from = fromUnit (to {symbol=p ++ symbol to}) 1
        scale = Scalar x (Just $ fromUnit to 1) / fromUnits from

conversions = all ++ map recipConversion all
  where
    all = baseConversions ++ siConversions

recipConversion (Conv from to scale) = Conv to from $ recip scale

conversionUnits = nub $ concat [[from, to] | Conv from to _ <- conversions]
