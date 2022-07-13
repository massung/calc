module Calc.Scalar where

import Calc.Units

data Scalar = Scalar Rational Units
  deriving (Eq)

instance FromUnits Scalar where
  fromUnits = Scalar 1

instance FromUnit Scalar where
  fromUnit u = fromUnits $ fromUnit u

instance Show Scalar where
  show (Scalar x u)
    | nullUnits u = show (fromRational x)
    | otherwise = show (fromRational x) ++ " " ++ show u

instance Num Scalar where
  fromInteger n = Scalar (fromInteger n) noUnits

  -- add scalars
  (+) (Scalar x ux) (Scalar y uy)
    | nullUnits uy = Scalar (x + y) ux
    | nullUnits ux = Scalar (x + y) uy
    | ux == uy = Scalar (x + y) ux
    | otherwise = error "Cannot add disparate units"

  -- multiply scalars
  (*) (Scalar x ux) (Scalar y uy) = Scalar (x * y) (ux <> uy)

  -- mapped functions
  negate = mapScalar negate
  abs = mapScalar abs
  signum = mapScalar signum

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) noUnits

  -- scalar inverse
  recip (Scalar x u) = Scalar (recip x) $ mapUnits negate u

scalarUnits (Scalar _ units) = units

mapScalar f (Scalar x u) = Scalar (f x) u

expScalar (Scalar x u) e = Scalar (x ^^ e) (mapUnits (* e) u)
