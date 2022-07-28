module Calc.Error where

import Calc.Units
import Control.Exception
import Text.Parsec

data Error
  = NoExpr
  | NoAnswer
  | NoFunction String
  | ExprError ParseError
  | ConversionError Units Units
  | WrongArity
  | IllegalExponent
  deriving (Eq)

instance Exception Error

instance Show Error where
  show (NoFunction f) = unwords ["no function:", f]
  show (ExprError e) = show e
  show (ConversionError from to) = unwords ["no conversion:", show from, "to", show to]
  show NoExpr = "no expression"
  show NoAnswer = "no answer"
  show WrongArity = "wrong arity"
  show IllegalExponent = "illegal exponent"
