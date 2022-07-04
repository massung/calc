{-# LANGUAGE OverloadedStrings #-}

module Calc.Dim where

import Data.Csv
import Data.Map.Strict as M

data Dim
  = Angle
  | Area
  | Duration
  | Energy
  | Length
  | Mass
  | Power
  | Pressure
  | Volume
  deriving (Eq, Ord, Show)

instance FromField Dim where
  parseField s
    | s == "angle" = pure Angle
    | s == "area" = pure Area
    | s == "duration" = pure Duration
    | s == "energy" = pure Energy
    | s == "length" = pure Length
    | s == "mass" = pure Mass
    | s == "power" = pure Power
    | s == "pressure" = pure Pressure
    | s == "volume" = pure Volume
    | otherwise = fail $ show s ++ " ?"

newtype Dims = Dims (Map Dim Integer)
  deriving (Eq, Ord, Show)

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/= 0) $ unionWith (+) a b

dims = Dims . fromListWith (+)
