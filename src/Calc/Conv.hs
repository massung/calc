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
import Data.List

data Conv = Conv Units Scalar
  deriving (Show)

instance FromNamedRecord Conv where
  parseNamedRecord r = do
    from <- r .: "from"
    to <- r .: "to"
    return $ Conv from to

conversions :: [Conv]
conversions = fromRight (error "ack!") $ F.toList . snd <$> decodeByName csv
  where
    csv = $(embedStringFile "units/conv.csv")

derivedConvs = [derived u p | u <- siUnits, p <- siPrefixes]
  where
    derived u (prefix, x) =
      let to = fromUnit u { symbol= prefix ++ symbol u } 1
       in Conv (fromUnit u 1) (Scalar x $ Just to)

recipConv (Conv from (Scalar x to)) =
  let from' = maybe (error "ACK!") recipUnits to
   in Conv from' $ Scalar (recip x) (Just $ recipUnits from)

conversionUnits = nub $ concat [[from, to] | Conv from (Scalar _ (Just to)) <- conversions]

conversionDims = nub $ concat [dims from to | Conv from (Scalar _ (Just to)) <- conversions]
  where
    dims from to = [unitsDims from, unitsDims to]
