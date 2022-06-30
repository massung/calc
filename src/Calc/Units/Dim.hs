{-# LANGUAGE OverloadedStrings #-}

module Calc.Units.Dim where

import Data.Csv
import Data.Map as M

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
  | Temperature
  | Volume
  deriving (Eq, Ord, Show)

newtype Dims = Dims (Dim, Double)
  deriving (Show)

instance Eq Dims where
  -- allow conversion <-> Length and Area
  (==) (Dims (Area, a)) (Dims (Length, b)) = a == b * 2
  (==) (Dims (Length, a)) (Dims (Area, b)) = a * 2 == b
  -- allow conversion <-> Length and Volume
  (==) (Dims (Volume, a)) (Dims (Length, b)) = a == b * 3
  (==) (Dims (Length, a)) (Dims (Volume, b)) = a * 3 == b
  -- do dimensions match?
  (==) (Dims a) (Dims b) = a == b

instance FromField Dim where
  parseField s
    | s == "angle" = pure Angle
    | s == "area" = pure Area
    | s == "duration" = pure Duration
    | s == "energy" = pure Energy
    | s == "force" = pure Force
    | s == "frequency" = pure Frequency
    | s == "length" = pure Length
    | s == "mass" = pure Mass
    | s == "power" = pure Power
    | s == "pressure" = pure Pressure
    | s == "speed" = pure Speed
    | s == "storage" = pure Storage
    | s == "temperature" = pure Temperature
    | s == "volume" = pure Volume
    | otherwise = fail $ show s
