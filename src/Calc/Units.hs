{-# LANGUAGE OverloadedStrings #-}

module Calc.Units where

import Calc.Dim
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

newtype Units = Units (Map Unit Integer)
  deriving (Eq, Ord)

instance Semigroup Units where
  (<>) (Units a) (Units b) = Units $ M.filter (/= 0) $ unionWith (+) a b

instance Monoid Units where
  mempty = Units mempty

class FromUnit a where
  fromUnit :: Unit -> a

class FromUnits a where
  fromUnits :: Units -> a

instance FromUnit Units where
  fromUnit u = Units $ M.singleton u 1

instance Show Units where
  show (Units u)
    | F.null num = showUnits den
    | F.null den = showUnits num
    | otherwise = showUnits num ++ "/" ++ showUnits (M.map abs den)
    where
      (num, den) = M.partition (> 0) u

      -- display a single unit with exponent
      showUnit (u, 1) = show u
      showUnit (u, n) = show u ++ "^" ++ show n

      -- concatenate units together
      showUnits = unwords . L.map showUnit . M.toList

imperialUnits =
  [ Unit {name = "inch", symbol = "in", dim = Length},
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
    Unit {name = "pounds per sqin", symbol = "psi", dim = Pressure},
    Unit {name = "bar", symbol = "bar", dim = Pressure}
  ]

metricUnits =
  [ Unit {name = "second", symbol = "s", dim = Duration},
    Unit {name = "gram", symbol = "g", dim = Mass},
    Unit {name = "meter", symbol = "m", dim = Length},
    Unit {name = "liter", symbol = "L", dim = Volume},
    Unit {name = "pascal", symbol = "Pa", dim = Pressure}
  ]

storageUnits =
  [ Unit {name = "bit", symbol = "b", dim = Storage},
    Unit {name = "byte", symbol = "B", dim = Storage}
  ]

siUnits = [derivedUnit u n p | u <- metricUnits, (n, p, _) <- siPrefixes]

siStorageUnits = [derivedUnit u n p | u <- storageUnits, (n, p, _) <- siStoragePrefixes]

derivedUnit u n p = u {name = n ++ name u, symbol = p ++ symbol u}

unitMap :: Map String Unit
unitMap = F.foldl' insert mempty $ concat [imperialUnits, metricUnits, siUnits, storageUnits, siStorageUnits]
  where
    insert m u = M.insert (symbol u) u m

units :: [Units]
units = L.map (fromUnit . snd) $ M.toList unitMap

mapUnits f (Units u) = Units (M.map f u)

recipUnits = mapUnits negate

divideUnits a b = mappend a (recipUnits b)

simplify m = (M.map (`div` factor) m, factor)
  where
    factor =
      let x = M.foldl' gcd (maximum m) m
       in if all (< 0) m then negate x else x

simplifyUnits (Units u) = first Units $ simplify u
