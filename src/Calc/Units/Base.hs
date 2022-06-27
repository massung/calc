{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Units.Base where

import Data.Csv
import Data.Either
import Data.FileEmbed
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.String
import Data.Vector as V

data Unit = Unit {name :: Maybe String, symbol :: String}
  deriving (Eq, Ord)

instance Show Unit where
  show = symbol

instance FromNamedRecord Unit where
  parseNamedRecord r = do
    name <- r .: "name"
    symbol <- r .: "symbol"
    return $ Unit {name = Just name, symbol = symbol}

instance IsString Unit where
  fromString s = case M.lookup s unitsMap of
    Nothing -> error $ "unknown units: " L.++ s
    Just unit -> unit

siPrefixes =
  [ ("yocto", "y", 1e-24),
    ("zepto", "z", 1e-21),
    ("atto", "a", 1e-18),
    ("femto", "f", 1e-15),
    ("pico", "p", 1e-12),
    ("nano", "n", 1e-9),
    ("micro", "u", 1e-6),
    ("milli", "m", 1e-3),
    ("centi", "c", 1e-2),
    ("deci", "d", 1e-1),
    ("deka", "da", 1e1),
    ("hecto", "h", 1e2),
    ("kilo", "k", 1e3),
    ("mega", "M", 1e6),
    ("giga", "G", 1e9),
    ("tera", "T", 1e12),
    ("peta", "P", 1e15),
    ("exa", "E", 1e18),
    ("zetta", "Z", 1e21),
    ("yotta", "Y", 1e24)
  ]

mkSIUnits u = u : L.map mkUnits siPrefixes
  where
    mkUnits (prefixName, prefixSymbol, _) =
      Unit
        { name = (prefixName L.++) <$> name u,
          symbol = prefixSymbol L.++ symbol u
        }

imperialUnitsCsv = $(embedStringFile "res/units.csv")

siUnitsCsv = $(embedStringFile "res/siUnits.csv")

imperialUnits = V.foldl insert M.empty . snd <$> decodeByName imperialUnitsCsv
  where
    insert m u = M.insert (symbol u) u m

siUnits = V.foldl insert M.empty . snd <$> decodeByName siUnitsCsv
  where
    insert m u = L.foldl (\m u -> M.insert (symbol u) u m) m $ mkSIUnits u

unitsMap = do
  a <- imperialUnits
  b <- siUnits
  return $ M.unions [a, b]
