module Calc.Error where

import Calc.Units
import Text.Parsec

data Error
  = ExprError ParseError
  | ConversionError Units Units

instance Show Error where
  show (ExprError e) = show e
  show (ConversionError from to) = unwords ["no conversion:", show from, "to", show to]
