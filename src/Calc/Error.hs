module Calc.Error where

import Calc.Units
import Control.Exception
import Text.Parsec

data Error
  = NoExpr
  | ExprError ParseError
  | ConversionError Units Units

instance Exception Error where

instance Show Error where
  show (ExprError e) = show e
  show (ConversionError from to) = unwords ["no conversion:", show from, "to", show to]
  show NoExpr = "no expression"
