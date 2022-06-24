module Calc.Scalar where

import Calc.Units

data Scalar = Scalar Double (Maybe Units)

instance Show Scalar where
  show (Scalar n Nothing) = show n
  show (Scalar n (Just u)) = show n ++ " " ++ show u

instance Eq Scalar where
  (==) (Scalar n1 u1) (Scalar n2 u2) = u1 == u2 && n1 == n2

instance Ord Scalar where
  (<=) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then n1 <= n2
      else error "Cannot compare disparately typed scalars."

instance Num Scalar where
  -- adding scalars with the same units
  (+) (Scalar n1 Nothing) (Scalar n2 Nothing) =
    Scalar (n1 + n2) Nothing
  (+) (Scalar n1 (Just u1)) (Scalar n2 Nothing) =
    Scalar (n1 + n2) (Just u1)
  (+) (Scalar n1 Nothing) (Scalar n2 (Just u2)) =
    Scalar (n1 + n2) (Just u2)
  (+) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then Scalar (n1 + n2) u1
      else error "Cannot add disparately typed scalars."

  -- subtracting scalars with the same units
  (-) (Scalar n1 Nothing) (Scalar n2 Nothing) =
    Scalar (n1 - n2) Nothing
  (-) (Scalar n1 (Just u1)) (Scalar n2 Nothing) =
    Scalar (n1 - n2) (Just u1)
  (-) (Scalar n1 Nothing) (Scalar n2 (Just u2)) =
    Scalar (n1 - n2) (Just u2)
  (-) (Scalar n1 u1) (Scalar n2 u2) =
    if u1 == u2
      then Scalar (n1 - n2) u1
      else error "Cannot subtract disparately typed scalars."

  -- multiplication of scalars
  (*) (Scalar n1 Nothing) (Scalar n2 Nothing) =
    Scalar (n1 * n2) Nothing
  (*) (Scalar n1 (Just u1)) (Scalar n2 Nothing) =
    Scalar (n1 * n2) (Just u1)
  (*) (Scalar n1 Nothing) (Scalar n2 (Just u2)) =
    Scalar (n1 * n2) (Just u2)
  (*) (Scalar n1 (Just u1)) (Scalar n2 (Just u2)) =
    Scalar (n1 * n2) (Just $ multiplyUnits u1 u2)

  -- create a new scalar from an integer
  fromInteger i = Scalar (fromInteger i) Nothing

  -- numeric operations on scalars
  negate (Scalar n u) = Scalar (negate n) u
  abs (Scalar n u) = Scalar (abs n) u
  signum (Scalar n u) = Scalar (signum n) u

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) Nothing

  -- divide scalars and simplify units
  (/) (Scalar n1 Nothing) (Scalar n2 Nothing) =
    Scalar (n1 / n2) Nothing
  (/) (Scalar n1 (Just u1)) (Scalar n2 Nothing) =
    Scalar (n1 / n2) (Just u1)
  (/) (Scalar n1 Nothing) (Scalar n2 (Just u2)) =
    Scalar (n1 / n2) (Just $ recipUnits u2)
  (/) (Scalar n1 (Just u1)) (Scalar n2 (Just u2)) =
    Scalar (n1 / n2) (Just $ divideUnits u1 u2)

  -- reciprocal of the scalar
  recip (Scalar n Nothing) = Scalar (recip n) Nothing
  recip (Scalar n (Just u)) = Scalar (recip n) (Just $ recipUnits u)

expScalar (Scalar _ _) (Scalar _ (Just _)) = error "Cannot add units to exponent."
expScalar (Scalar x Nothing) (Scalar n _) = Scalar (x ** n) Nothing
expScalar (Scalar x (Just u)) (Scalar n _) = Scalar (x ** n) $ Just (expUnits u n)
