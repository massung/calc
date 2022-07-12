{-# LANGUAGE OverloadedStrings #-}

module Calc.Dim where

import Data.Map.Strict as M

data Dim
  = Angle
  | Area
  | Duration
  | Energy
  | Frequency
  | Length
  | Mass
  | Power
  | Pressure
  | Storage
  | Volume
  deriving (Eq, Ord, Show)

newtype Dims = Dims (Map Dim Integer)
  deriving (Eq, Ord, Show)

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/= 0) $ unionWith (+) a b

dims = Dims . fromListWith (+)
