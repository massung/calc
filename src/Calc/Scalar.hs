module Calc.Scalar where

import Calc.Units
import Data.Scientific

data Scalar = Scalar Scientific Units

instance Show Scalar where
  show (Scalar n u) = show n ++ " " ++ show u

instance Eq Scalar where
  (==) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then n1 == n2
      else error "Cannot compare disparately typed scalars."

instance Ord Scalar where
  (<=) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then n1 <= n2
      else error "Cannot compare disparately typed scalars."

instance Num Scalar where
  -- adding scalars with the same units
  (+) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then Scalar (n1 + n2) u1
      else error "Cannot add disparately typed scalars."

  -- subtracting scalars with the same units
  (-) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then Scalar (n1 - n2) u1
      else error "Cannot subtract disparately typed scalars."

  -- multiplication of scalars
  (*) (Scalar n1 u1) (Scalar n2 u2) =
    Scalar (n1 * n2) (multiplyUnits u1 u2)

  -- create a new scalar from an integer
  fromInteger i = Scalar (fromInteger i) emptyUnits

  -- numeric operations on scalars
  negate (Scalar n u) = Scalar (negate n) u
  abs (Scalar n u) = Scalar (abs n) u
  signum (Scalar n u) = Scalar (signum n) u

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) emptyUnits

  -- divide scalars and simplify units
  (/) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then Scalar (n1 / n2) emptyUnits
      else Scalar (n1 / n2) (multiplyUnits u1 $ recipUnits u2)

  -- reciprocal of the scalar
  recip (Scalar n u) = Scalar (recip n) u
