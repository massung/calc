{-# LANGUAGE OverloadedStrings #-}

module Calc.Units.Dim where

import Data.Csv

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
