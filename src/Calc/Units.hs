{-# LANGUAGE OverloadedStrings #-}

module Calc.Units where

import Calc.Dim
import Calc.Exps
import Calc.SI
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.String
import Data.Tuple.Extra

data Unit = Unit {name :: String, symbol :: String, dim :: Dim}
  deriving (Eq, Ord)

instance IsString Unit where
  fromString s = fromMaybe (error $ "no unit " ++ show s) $ M.lookup s unitMap

instance Show Unit where
  show = symbol

newtype Units = Units (Exps Unit)
  deriving (Eq, Ord)

instance Semigroup Units where
  (<>) (Units a) (Units b) = Units $ appendExps a b

instance Monoid Units where
  mempty = Units mempty

class FromUnit a where
  fromUnit :: Unit -> a

class FromUnits a where
  fromUnits :: Units -> a

instance FromUnit Units where
  fromUnit u = Units $ M.singleton u 1

instance Show Units where
  show (Units u) = showExps u

imperialUnits =
  [ Unit {name = "mil", symbol = "mil", dim = Length},
    Unit {name = "inch", symbol = "in", dim = Length},
    Unit {name = "hand", symbol = "h", dim = Length},
    Unit {name = "foot", symbol = "ft", dim = Length},
    Unit {name = "yard", symbol = "yd", dim = Length},
    Unit {name = "chain", symbol = "ch", dim = Length},
    Unit {name = "furlong", symbol = "fur", dim = Length},
    Unit {name = "mile", symbol = "mi", dim = Length},
    Unit {name = "league", symbol = "lea", dim = Length},
    Unit {name = "fathom", symbol = "ftm", dim = Length},
    Unit {name = "cable", symbol = "cable", dim = Length},
    Unit {name = "nautical mile", symbol = "nmi", dim = Length},
    Unit {name = "link", symbol = "link", dim = Length},
    Unit {name = "rod", symbol = "rod", dim = Length},
    Unit {name = "acre", symbol = "acre", dim = Area},
    Unit {name = "hectare", symbol = "ha", dim = Area},
    Unit {name = "teaspoon", symbol = "tsp", dim = Volume},
    Unit {name = "tablespoon", symbol = "tbsp", dim = Volume},
    Unit {name = "fluid ounce", symbol = "floz", dim = Volume},
    Unit {name = "gill", symbol = "gill", dim = Volume},
    Unit {name = "cup", symbol = "cup", dim = Volume},
    Unit {name = "pint", symbol = "pt", dim = Volume},
    Unit {name = "quart", symbol = "qt", dim = Volume},
    Unit {name = "gallon", symbol = "gal", dim = Volume},
    Unit {name = "ounce", symbol = "oz", dim = Mass},
    Unit {name = "pound", symbol = "lb", dim = Mass},
    Unit {name = "stone", symbol = "st", dim = Mass},
    Unit {name = "slug", symbol = "slug", dim = Mass},
    Unit {name = "quarter", symbol = "qtr", dim = Mass},
    Unit {name = "hundredweight", symbol = "cwt", dim = Mass},
    Unit {name = "ton", symbol = "t", dim = Mass},
    Unit {name = "minute", symbol = "min", dim = Duration},
    Unit {name = "hour", symbol = "hr", dim = Duration},
    Unit {name = "day", symbol = "day", dim = Duration},
    Unit {name = "horsepower", symbol = "HP", dim = Power},
    Unit {name = "pounds per sq. inch", symbol = "psi", dim = Pressure},
    Unit {name = "bar", symbol = "bar", dim = Pressure},
    Unit {name = "british thermal unit", symbol = "BTU", dim = Energy},
    Unit {name = "pound force", symbol = "lbf", dim = Force},
    Unit {name = "knot", symbol = "knot", dim = Speed}
  ]

metricUnits =
  [ Unit {name = "second", symbol = "s", dim = Duration},
    Unit {name = "gram", symbol = "g", dim = Mass},
    Unit {name = "meter", symbol = "m", dim = Length},
    Unit {name = "liter", symbol = "L", dim = Volume},
    Unit {name = "pascal", symbol = "Pa", dim = Pressure},
    Unit {name = "hertz", symbol = "hz", dim = Frequency},
    Unit {name = "watt", symbol = "W", dim = Power},
    Unit {name = "joule", symbol = "J", dim = Energy},
    Unit {name = "newton", symbol = "N", dim = Force},
    Unit {name = "volt", symbol = "V", dim = Voltage},
    Unit {name = "coulomb", symbol = "C", dim = Current},
    Unit {name = "ampere", symbol = "A", dim = Charge},
    Unit {name = "ohm", symbol = "O", dim = Resistance},
    Unit {name = "farad", symbol = "F", dim = Capacitance}
  ]

angleUnits =
  [ Unit {name = "radian", symbol = "rad", dim = Angle},
    Unit {name = "degree", symbol = "deg", dim = Angle},
    Unit {name = "revolution", symbol = "rev", dim = Angle},
    Unit {name = "arcsecond", symbol = "arcs", dim = Angle},
    Unit {name = "arcminute", symbol = "arcm", dim = Angle}
  ]

storageUnits =
  [ Unit {name = "bit", symbol = "b", dim = Storage},
    Unit {name = "byte", symbol = "B", dim = Storage}
  ]

siUnits = [derivedUnit u n p | u <- metricUnits, (n, p, _) <- siPrefixes]

siStorageUnits = [derivedUnit u n p | u <- storageUnits, (n, p, x) <- siPrefixes, x > 1]

derivedUnit u n p = u {name = n ++ name u, symbol = p ++ symbol u}

unitMap :: Map String Unit
unitMap = F.foldl' (\m u -> M.insert (symbol u) u m) mempty allUnits
  where
    allUnits =
      concat
        [ imperialUnits,
          metricUnits,
          angleUnits,
          siUnits,
          storageUnits,
          siStorageUnits
        ]

units :: [Units]
units = L.map (fromUnit . snd) $ M.toList unitMap

fromUnitList = Units . fromList

nullUnits (Units u) = M.null u

mapUnits f (Units u) = Units (M.map f u)

recipUnits = mapUnits negate

divideUnits a b = mappend a (recipUnits b)

simplifyUnits (Units u) = first Units $ simplify u

dims (Units u) = Dims $ M.mapKeys dim u

dims' (Units u) = M.fromList [unitDims u' | u' <- M.toList u]
  where
    unitDims u@(u', e) = (dim u', Units $ M.singleton u' e)
