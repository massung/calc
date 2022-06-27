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
  parseField r
    | r == "angle" = pure Angle
    | r == "area" = pure Area
    | r == "duration" = pure Duration
    | r == "energy" = pure Energy
    | r == "force" = pure Force
    | r == "frequency" = pure Frequency
    | r == "length" = pure Length
    | r == "mass" = pure Mass
    | r == "power" = pure Power
    | r == "pressure" = pure Pressure
    | r == "speed" = pure Speed
    | r == "storage" = pure Storage
    | r == "temperature" = pure Temperature
    | r == "volume" = pure Volume
    | otherwise = fail $ show r
