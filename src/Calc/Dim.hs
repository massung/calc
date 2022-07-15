{-# LANGUAGE OverloadedStrings #-}

module Calc.Dim where

import Calc.Exps

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
  deriving (Eq, Ord)

instance Show Dim where
  show Angle = "[angle]"
  show Area = "[area]"
  show Capacitance = "[capacitance]"
  show Charge = "[charge]"
  show Current = "[current]"
  show Duration = "[duration]"
  show Energy = "[energy]"
  show Force = "[force]"
  show Frequency = "[frequency]"
  show Length = "[length]"
  show Mass = "[mass]"
  show Power = "[power]"
  show Pressure = "[pressure]"
  show Resistance = "[resistance]"
  show Speed = "[speed]"
  show Storage = "[storage]"
  show Voltage = "[voltage]"
  show Volume = "[volume]"

newtype Dims = Dims (Exps Dim)
  deriving (Eq, Ord)

instance Show Dims where
  show (Dims dims) = showExps dims

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ appendExps a b

instance Monoid Dims where
  mempty = Dims mempty
