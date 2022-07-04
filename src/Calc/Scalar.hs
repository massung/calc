module Calc.Scalar where

import Calc.Units
import Data.Csv

data Scalar = Scalar Double (Maybe Units)
  deriving (Eq)

instance FromUnits Scalar where
  fromUnits = Scalar 1 . Just

instance FromUnit Scalar where
  fromUnit u e = fromUnits $ fromUnit u e

instance Show Scalar where
  show (Scalar x Nothing) = show x
  show (Scalar x (Just u)) = show x ++ " " ++ show u

instance Num Scalar where
  fromInteger n = Scalar (fromInteger n) Nothing

  -- add scalars
  (+) (Scalar x ux) (Scalar y uy)
    | ux == uy = Scalar (x + y) ux
    | otherwise = error "Cannot add disparate units"

  -- multiply scalars
  (*) (Scalar x Nothing) (Scalar y uy) = Scalar (x * y) uy
  (*) (Scalar x ux) (Scalar y Nothing) = Scalar (x * y) ux
  (*) (Scalar x (Just ux)) (Scalar y (Just uy)) = scalar (x * y) (ux <> uy)

  -- mapped functions
  negate = mapScalar negate
  abs = mapScalar abs
  signum = mapScalar signum

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) Nothing

  -- scalar inverse
  recip (Scalar x u) = Scalar (recip x) $ fmap (mapUnits negate) u

scalar x = Scalar x . Just

scalarUnits (Scalar _ units) = units

mapScalar f (Scalar x u) = Scalar (f x) u

expScalar (Scalar x Nothing) e = Scalar (x ** fromInteger e) Nothing
expScalar (Scalar x (Just u)) e = Scalar (x ** fromInteger e) (Just $ mapUnits (* e) u)
