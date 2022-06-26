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

data Conv = Conv {from :: Unit, to :: Units, scale :: Double}
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
    units = multiplyUnits (to conv) (U.singleton (from conv) -1)

-- parseSIConversions csvBS = do
--   (_, rows) <- decodeByName csvBS :: Either String (Header, Vector SIUnits)
--   return $ L.concatMap convs $ V.toList rows
--   where
--     convs (SIUnits (_, u)) =
--       [ conv "z" u 1e-24,
--         conv "y" u 1e-21,
--         conv "a" u 1e-18,
--         conv "f" u 1e-15,
--         conv "p" u 1e-12,
--         conv "n" u 1e-9,
--         conv "u" u 1e-6,
--         conv "m" u 1e-3,
--         conv "c" u 1e-2,
--         conv "d" u 1e-1,
--         conv "da" u 10,
--         conv "h" u 1e2,
--         conv "k" u 1e3,
--         conv "M" u 1e6,
--         conv "G" u 1e9,
--         conv "T" u 1e12,
--         conv "P" u 1e15,
--         conv "E" u 1e18,
--         conv "Z" u 1e21,
--         conv "Y" u 1e24
--       ]

--     -- apply si prefix to base units with scale factor
--     conv prefix u scale =
--       Conv {from = Unit $ prefix ++ u, to = Unit u, scale = scale}
