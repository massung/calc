{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Units where

import Calc.Dim
import Data.Csv
import Data.Either
import Data.FileEmbed
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.String
import Data.Tuple.Extra

data Unit = Unit {symbol :: String, dim :: Dim}
  deriving (Eq, Ord)

instance IsString Unit where
  fromString = (unitsMap !)

instance Show Unit where
  show = symbol

instance FromNamedRecord Unit where
  parseNamedRecord r = do
    dim <- r .: "dim"
    symbol <- r .: "symbol"
    return $ Unit {symbol = symbol, dim = dim}

newtype Units = Units (Map Unit Integer)
  deriving (Eq, Ord)

instance Semigroup Units where
  (<>) (Units a) (Units b) = Units $ M.filter (/= 0) $ unionWith (+) a b

instance Monoid Units where
  mempty = Units mempty

class FromUnit a where
  fromUnit :: Unit -> Integer -> a

class FromUnits a where
  fromUnits :: Units -> a

instance FromUnit Units where
  fromUnit u e = Units $ M.singleton u e

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

imperialUnits = fromRight (fail "ack!") $ F.toList . snd <$> decodeByName csv
  where
    csv = $(embedStringFile "units/imperial.csv")

siUnits = fromRight (error "ack!") $ F.toList . snd <$> decodeByName csv
  where
    csv = $(embedStringFile "units/si.csv")

siPrefixes =
  [ ("a", 1e-18),
    ("f", 1e-15),
    ("p", 1e-12),
    ("n", 1e-9),
    ("u", 1e-6),
    ("m", 1e-3),
    ("c", 1e-2),
    ("d", 1e-1),
    ("da", 1e1),
    ("h", 1e2),
    ("k", 1e3),
    ("M", 1e6),
    ("G", 1e9),
    ("T", 1e12),
    ("P", 1e15),
    ("E", 1e18)
  ]

derivedUnits = [derived u p | u <- siUnits, p <- siPrefixes]
  where
    derived u (p, _) = u {symbol = p ++ symbol u}

unitsMap :: Map String Unit
unitsMap = F.foldl' insert mempty $ concat [imperialUnits, siUnits, derivedUnits]
  where
    insert m u = M.insert (show u) u m

units :: [Units]
units = nub [fromUnit u 1 | (_, u) <- M.toList unitsMap]

mapUnits f (Units u) = Units (M.map f u)

recipUnits = mapUnits negate

divideUnits a b = mappend a (recipUnits b)

unitsDims (Units u) = dims [(dim u, exp) | (u, exp) <- M.toList u]

simplify m = (M.map (`div` factor) m, factor)
  where
    factor = M.foldl' gcd (maximum m) m

simplifyUnits (Units u) = first Units $ simplify u

simplifyDims (Dims d) = first Dims $ simplify d
