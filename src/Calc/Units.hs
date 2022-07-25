{-# LANGUAGE OverloadedStrings #-}

module Calc.Units where

import Calc.Conv
import Calc.Dims
import Control.Applicative
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Ratio
import Data.String
import Data.Tuple.Extra

data Unit = Unit {dim :: Dim, symbol :: String, conv :: Conv}

instance Eq Unit where
  (==) a b = symbol a == symbol b

instance Ord Unit where
  compare a b = compare (symbol a) (symbol b)

instance IsString Unit where
  fromString s = fromMaybe (error $ "no unit " ++ show s) $ M.lookup s unitMap

instance Show Unit where
  show = symbol

newtype Units = Units (Map Unit Int)
  deriving (Eq, Ord)

instance Semigroup Units where
  (<>) (Units a) (Units b) = Units $ M.filter (/= 0) $ M.unionWith (+) a b

instance Monoid Units where
  mempty = Units mempty

instance Show Units where
  show (Units u)
    | F.null num = showExps' den
    | F.null den = showExps' num
    | otherwise = showExps' num ++ "/" ++ showExps' (M.map abs den)
    where
      (num, den) = M.partition (> 0) u

      -- display a single unit with exponent
      showExp (x, 1) = show x
      showExp (x, n) = show x ++ "^" ++ show n

      -- concatenate units together
      showExps' = unwords . L.map showExp . M.toList

{-
-- angle units
-}

_radian = Unit {dim = Angle, symbol = "rad", conv = Base}

_degree = Unit {dim = Angle, symbol = "deg", conv = Linear 57.2958}

_arcsec = Unit {dim = Angle, symbol = "arcs", conv = Linear 206265}

_arcmin = Unit {dim = Angle, symbol = "arcm", conv = Linear 3427.75}

_rev = Unit {dim = Angle, symbol = "rev", conv = Linear 0.1591549430919}

{-
-- area units
-}

_hectare = Unit {dim = Area, symbol = "ha", conv = Linear 1e-4}

_acre = Unit {dim = Area, symbol = "acre", conv = Linear 2.471e-4}

{-
-- duration units
-}

_second = Unit {dim = Duration, symbol = "s", conv = Base}

_minute = Unit {dim = Duration, symbol = "min", conv = Linear (1 % 60)}

_hour = Unit {dim = Duration, symbol = "hr", conv = Linear (1 % 3600)}

_day = Unit {dim = Duration, symbol = "day", conv = Linear (1 % 86400)}

{-
-- electrical units
-}

_farad = Unit {dim = Capacitance, symbol = "F", conv = Base}

_coulomb = Unit {dim = Charge, symbol = "C", conv = Base}

_ampere = Unit {dim = Current, symbol = "A", conv = Base}

_ohm = Unit {dim = Resistance, symbol = "O", conv = Base}

_volt = Unit {dim = Voltage, symbol = "V", conv = Base}

{-
-- energy units
-}

_joule = Unit {dim = Energy, symbol = "J", conv = Base}

_btu = Unit {dim = Energy, symbol = "BTU", conv = Linear 9.478e-4}

_therm = Unit {dim = Energy, symbol = "thm", conv = Linear 9.478e-9}

{-
-- force units
-}

_newton = Unit {dim = Force, symbol = "N", conv = Base}

_poundForce = Unit {dim = Force, symbol = "lbf", conv = Linear 0.2248}

-- length units
_meter = Unit {dim = Length, symbol = "m", conv = Base}

_mil = Unit {dim = Length, symbol = "mil", conv = Linear 39370}

_inch = Unit {dim = Length, symbol = "in", conv = Linear 39.37008}

_hand = Unit {dim = Length, symbol = "h", conv = Linear 9.3475}

_foot = Unit {dim = Length, symbol = "ft", conv = Linear 3.28084}

_yard = Unit {dim = Length, symbol = "yd", conv = Linear 1.0936133}

_fathom = Unit {dim = Length, symbol = "ftm", conv = Linear 0.5468}

_chain = Unit {dim = Length, symbol = "ch", conv = Linear 0.04971}

_furlong = Unit {dim = Length, symbol = "fur", conv = Linear 0.004971}

_mile = Unit {dim = Length, symbol = "mi", conv = Linear 6.214e-4}

_league = Unit {dim = Length, symbol = "lea", conv = Linear 2.071e-4}

_cable = Unit {dim = Length, symbol = "cable", conv = Linear 0.004557}

_nauticalMile = Unit {dim = Length, symbol = "nmi", conv = Linear 5.399568413e-4}

_link = Unit {dim = Length, symbol = "link", conv = Linear 4.970969886302}

_rod = Unit {dim = Length, symbol = "rod", conv = Linear 0.1988384}

{-
-- mass units
-}

_gram = Unit {dim = Mass, symbol = "g", conv = Linear 1000.0}

_ounce = Unit {dim = Mass, symbol = "oz", conv = Linear 35.274}

_pound = Unit {dim = Mass, symbol = "lb", conv = Linear 2.205}

_stone = Unit {dim = Mass, symbol = "st", conv = Linear 0.1575}

_slug = Unit {dim = Mass, symbol = "slug", conv = Linear 0.06852}

_quarter = Unit {dim = Mass, symbol = "qtr", conv = Linear 0.08818}

_hundredweight = Unit {dim = Mass, symbol = "cwt", conv = Linear 0.02205}

_ton = Unit {dim = Mass, symbol = "t", conv = Linear 0.0011}

{-
-- power units
-}

_watt = Unit {dim = Power, symbol = "W", conv = Base}

_horsepower = Unit {dim = Power, symbol = "hp", conv = Linear 0.001341}

{-
-- pressure units
-}

_pascal = Unit {dim = Pressure, symbol = "Pa", conv = Base}

_psi = Unit {dim = Pressure, symbol = "psi", conv = Linear 1.45e-4}

_bar = Unit {dim = Pressure, symbol = "bar", conv = Linear 1e-5}

{-
-- speed units
-}

_kph = Unit {dim = Speed, symbol = "kph", conv = Linear 3.600}

_knot = Unit {dim = Speed, symbol = "kn", conv = Linear 1.9438445}

_mph = Unit {dim = Speed, symbol = "mph", conv = Linear 2.2369363}

{-
-- storage units
-}

_bit = Unit {dim = Storage, symbol = "b", conv = Linear 8}

_byte = Unit {dim = Storage, symbol = "B", conv = Base}

_kilobyte = Unit {dim = Storage, symbol = "kB", conv = Linear (1 % 1024)}

_megabyte = Unit {dim = Storage, symbol = "MB", conv = Linear (1 % 1048576)}

_gigabyte = Unit {dim = Storage, symbol = "GB", conv = Linear (1 % 1073741824)}

_terabyte = Unit {dim = Storage, symbol = "TB", conv = Linear (1 % 1099511627776)}

_petabyte = Unit {dim = Storage, symbol = "PB", conv = Linear (1 % 1125899906842624)}

_exabyte = Unit {dim = Storage, symbol = "EB", conv = Linear (1 % 1152921504606846976)}

{-
-- volume units
-}

_liter = Unit {dim = Volume, symbol = "L", conv = Linear 1000}

_teaspoon = Unit {dim = Volume, symbol = "tsp", conv = Linear 202884}

_tablespoon = Unit {dim = Volume, symbol = "tbsp", conv = Linear 67628}

_fluidOunce = Unit {dim = Volume, symbol = "floz", conv = Linear 33814}

_cup = Unit {dim = Volume, symbol = "c", conv = Linear 4226.7528}

_pint = Unit {dim = Volume, symbol = "pt", conv = Linear 2113.3764}

_quart = Unit {dim = Volume, symbol = "qt", conv = Linear 1057}

_gallon = Unit {dim = Volume, symbol = "gal", conv = Linear 264.1721}

siUnits u = [siUnit conversion | conversion <- siConversions]
  where
    siUnit (_, p, c) = u {symbol = p ++ symbol u, conv = conv u <> c}

unitMap :: Map String Unit
unitMap = F.foldl' (\m u -> M.insert (symbol u) u m) mempty units
  where
    units =
      concat
        [ [ _acre,
            _ampere,
            _arcmin,
            _arcsec,
            _bar,
            _bit,
            _btu,
            _byte,
            _cable,
            _chain,
            _coulomb,
            _cup,
            _day,
            _degree,
            _exabyte,
            _farad,
            _fathom,
            _fluidOunce,
            _foot,
            _furlong,
            _gallon,
            _gigabyte,
            _gram,
            _hand,
            _hectare,
            _horsepower,
            _hour,
            _hundredweight,
            _inch,
            _joule,
            _kilobyte,
            _knot,
            _kph,
            _league,
            _link,
            _liter,
            _megabyte,
            _meter,
            _mil,
            _mile,
            _minute,
            _mph,
            _nauticalMile,
            _newton,
            _ohm,
            _ounce,
            _pascal,
            _petabyte,
            _pint,
            _pound,
            _poundForce,
            _psi,
            _quart,
            _quarter,
            _radian,
            _rev,
            _rod,
            _second,
            _slug,
            _stone,
            _tablespoon,
            _teaspoon,
            _terabyte,
            _therm,
            _ton,
            _volt,
            _watt,
            _yard
          ],
          -- si units
          siUnits _ampere,
          siUnits _coulomb,
          siUnits _farad,
          siUnits _gram,
          siUnits _joule,
          siUnits _liter,
          siUnits _meter,
          siUnits _newton,
          siUnits _ohm,
          siUnits _pascal,
          siUnits _second,
          siUnits _volt,
          siUnits _watt
        ]

nullUnits (Units u) = M.null u

dims (Units u) = mconcat [powDims n $ baseDims (dim unit) | (unit, n) <- M.toList u]

mapUnits f (Units u) = Units $ M.map f u

recipUnits = mapUnits negate

(</>) a b = a <> recipUnits b

simplify (Units m) = (M.map (`div` factor) m, factor)
  where
    factor =
      let x = M.foldl' gcd (maximum m) m
       in if all (< 0) m then negate x else x

simplifyUnits u = first Units $ simplify u

validateUnits (Units u) = all (== 1) $ M.foldlWithKey' countDims M.empty u
  where
    countDims m u _ = M.alter (\x -> (+ 1) <$> x <|> Just 1) (dim u) m
