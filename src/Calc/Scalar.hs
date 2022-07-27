{-# LANGUAGE MultiWayIf #-}

module Calc.Scalar where

import Calc.Conv
import Calc.Dims
import Calc.Error
import Calc.Units
import Data.Foldable as F
import Data.Map.Strict as M
import Text.Printf

data Scalar = Scalar Rational Dims Units
  deriving (Eq, Ord)

instance Show Scalar where
  show (Scalar x d u)
    | nullUnits u = show (fromRational x)
    | otherwise = show (fromRational x) ++ " " ++ show u

instance PrintfArg Scalar where
  formatArg (Scalar x d u) fmt
    | fmtChar (vFmt 'g' fmt) == 'g' = formatRealFloat (fromRational x) fmt
    | fmtChar (vFmt 'U' fmt) == 'U' = formatString (show u) fmt {fmtChar = 's'}
    | otherwise = errorBadFormat $ fmtChar fmt

instance Semigroup Scalar where
  (<>) a b = a * b

instance Num Scalar where
  fromInteger n = Scalar (fromInteger n) mempty mempty

  -- add scalars
  (+) (Scalar x dx ux) (Scalar y dy uy)
    | nullDims dy = Scalar (x + y) dx ux
    | nullDims dx = Scalar (x + y) dy uy
    | dx == dy = Scalar (x + y) dx ux
    | otherwise = error "Cannot add disparate units"

  -- multiply scalars
  (*) (Scalar x dx ux) (Scalar y dy uy) = Scalar (x * y) (dx <> dy) (ux <> uy)

  -- mapped functions
  negate = mapScalar negate
  abs = mapScalar abs
  signum = mapScalar signum

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) mempty mempty

  -- scalar inverse
  recip (Scalar x d u) = Scalar (recip x) (recipDims d) (recipUnits u)

scalar x units = Scalar (toRational x) (dims units) units

mapScalar f (Scalar x d u) = Scalar (f x) d u

fromUnits u = Scalar 1 (dims u) u

harmonize (Scalar x d from) to
  | nullUnits to = Right $ Scalar x d from
  | nullDims d = Right $ Scalar x (dims to) to
  | otherwise = convert (Scalar x d from) $ harmonizeUnits from to

convert (Scalar x d from) to
  | nullUnits to = Right $ Scalar x d from
  | nullDims d = Right $ Scalar x (dims to) to
  | d == dims to = Right $ Scalar (applyConv (conversionScale from to) x) d to
  | otherwise = Left $ ConversionError from to

conversionScale (Units from) (Units to) = recipConv from' <> to'
  where
    from' = F.foldl' (<>) Base [powConv n conv | (Unit {conv = conv}, n) <- M.toList from]
    to' = F.foldl' (<>) Base [powConv n conv | (Unit {conv = conv}, n) <- M.toList to]
