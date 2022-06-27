{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Conv where

import Calc.Scalar
import Calc.Units.Base
import Calc.Units.Compound as U
import Calc.Units.Parser
import Data.ByteString.UTF8 as BS
import Data.Csv as Csv
import Data.Either
import Data.FileEmbed
import Data.Map as M
import Data.Vector as V
import Text.Parsec

data Conv = Conv {from :: Units, to :: Units, scale :: Double}
  deriving (Show)

instance FromField Unit where
  parseField s =
    let u = BS.toString s in case M.lookup u unitsMap of
      Nothing -> fail u
      Just unit -> pure unit

instance FromField Units where
  parseField s = case parse unitsParser "convs" $ BS.toString s of
    Left err -> fail $ show err
    Right conv -> pure conv

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
