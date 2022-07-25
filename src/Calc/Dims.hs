{-# LANGUAGE OverloadedLists #-}

module Calc.Dims where

import Data.Map.Strict as M

data Dim
  = Angle
  | Area
  | Capacitance
  | Charge
  | Current
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
  | Duration
  | Voltage
  | Volume
  deriving (Eq, Ord, Show)

newtype Dims = Dims (Map Dim Int)
  deriving (Eq, Ord, Show)

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/=0) $ M.unionWith (+) a b

instance Monoid Dims where
  mempty = Dims M.empty

baseDims :: Dim -> Dims
baseDims Angle = Dims [(Angle, 1)]
baseDims Area = Dims [(Length, 2)]
baseDims Capacitance = Dims [(Duration, 4), (Current, 2), (Mass, -1), (Length, -2)]
baseDims Charge = Dims [(Current, 1), (Duration, 1)]
baseDims Current = Dims [(Current, 1)]
baseDims Duration = Dims [(Duration, 1)]
baseDims Energy = Dims [(Mass, 1), (Length, 2), (Duration, -2)]
baseDims Force = Dims [(Mass, 1), (Length, 1), (Duration, -2)]
baseDims Frequency = Dims [(Duration, -1)]
baseDims Length = Dims [(Length, 1)]
baseDims Mass = Dims [(Mass, 1)]
baseDims Power = Dims [(Mass, 1), (Length, 2), (Duration, -3)]
baseDims Pressure = Dims [(Mass, 1), (Length, -1), (Duration, -2)]
baseDims Resistance = Dims [(Mass, 1), (Length, 2), (Duration, -3), (Current, -4)]
baseDims Speed = Dims [(Length, 1), (Duration, -1)]
baseDims Storage = Dims [(Storage, 1)]
baseDims Voltage = Dims [(Mass, 1), (Length, 2), (Duration, -3), (Current, -1)]
baseDims Volume = Dims [(Length, 3)]

nullDims (Dims dims) = M.null dims

mapDims f (Dims dims) = Dims $ M.map f dims

recipDims = mapDims negate
