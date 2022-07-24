module Calc.Dim where

import Data.Map.Strict as M

data Dim
  = Angle
  | Area
  | Capacitance
  | Charge
  | Current
  | Duration
  | Energy
  | Force
  | Frequency
  | Length
  | Mass
  | Power
  | Pressure
  | Resistance
  | Speed
  | Storage
  | Voltage
  | Volume
  deriving (Eq, Ord, Show)

newtype Dims = Dims (Map Dim Int)
  deriving (Eq, Ord, Show)
