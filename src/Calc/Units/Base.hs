{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Units.Base where

import Data.ByteString.UTF8 as BS
import Data.Csv
import Data.Either
import Data.FileEmbed
import Data.Maybe
import Data.String
import Data.Map as M
import Data.Vector as V

data Unit = Unit {name :: Maybe String, symbol :: String}
  deriving (Eq, Ord)

instance Show Unit where
  show = symbol

instance FromNamedRecord Unit where
  parseNamedRecord r = do
    name <- r .: "name"
    symbol <- r .: "symbol"
    return $ Unit {name=Just name, symbol=symbol}

instance IsString Unit where
  fromString s = fromMaybe unknown $ M.lookup s unitsMap
    where
      unknown = Unit {name=Nothing, symbol=s}

unitsCsv = $(embedStringFile "res/units.csv")

parsedUnits = V.foldl foldUnits M.empty . snd <$> decodeByName unitsCsv
  where
    foldUnits m u = insert (symbol u) u m

unitsMap = fromRight M.empty parsedUnits

units = [u | (_, u) <- M.toList unitsMap]
