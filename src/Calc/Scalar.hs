module Calc.Scalar where

import Calc.Units
import Text.Printf

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

instance PrintfArg Scalar where
  formatArg (Scalar x u) fmt
    | fmtChar (vFmt 'g' fmt) == 'g' = formatRealFloat (fromRational x) fmt
    | fmtChar (vFmt 'U' fmt) == 'U' = formatString (show u) fmt {fmtChar = 's'}
    | otherwise = errorBadFormat $ fmtChar fmt

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

--expScalar x@(Scalar _ _) (Scalar y ) = Scalar (x ^^ e) (mapUnits (* e) u)
