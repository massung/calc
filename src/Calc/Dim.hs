{-# LANGUAGE OverloadedStrings #-}

module Calc.Dim where

import Calc.Exps
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

newtype Dims = Dims (Exps Dim)
  deriving (Eq, Ord)

instance Show Dims where
  show (Dims dims) = showExps dims

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ appendExps a b

instance Monoid Dims where
  mempty = Dims mempty
