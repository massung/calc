module Calc.Error where

import Calc.Units
import Control.Exception
import Text.Parsec

data Error
  = NoExpr
  | NoAnswer
  | ExprError ParseError
  | ConversionError Units Units
  deriving (Eq)

instance Exception Error

instance Show Error where
  show (ExprError e) = show e
  show (ConversionError from to) = unwords ["no conversion:", show from, "to", show to]
  show NoExpr = "no expression"
  show NoAnswer = "no answer"
