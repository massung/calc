{-# LANGUAGE OverloadedStrings #-}

module Calc.Dim where

import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M

data Dim
  = Angle
  | Area
  | Duration
  | Energy
  | Force
  | Frequency
  | Length
  | Mass
  | Power
  | Pressure
  | Speed
  | Storage
  | Volume
  deriving (Eq, Ord)

instance Show Dim where
  show Angle = "[angle]"
  show Area = "[area]"
  show Duration = "[duration]"
  show Energy = "[energy]"
  show Force = "[force]"
  show Frequency = "[frequency]"
  show Length = "[length]"
  show Mass = "[mass]"
  show Power = "[power]"
  show Pressure = "[pressure]"
  show Speed = "[speed]"
  show Storage = "[storage]"
  show Volume = "[volume]"

newtype Dims = Dims (Map Dim Integer)
  deriving (Eq, Ord)

instance Show Dims where
  show (Dims dims)
    | F.null num = showDims den
    | F.null den = showDims num
    | otherwise = showDims num ++ "/" ++ showDims (M.map abs den)
    where
      (num, den) = M.partition (> 0) dims

      -- display a single unit with exponent
      showDim (d, 1) = show d
      showDim (d, n) = show d ++ "^" ++ show n

      -- concatenate units together
      showDims = unwords . L.map showDim . M.toList

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/= 0) $ unionWith (+) a b

dims = Dims . fromListWith (+)
