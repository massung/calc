module Calc.Error where

import Calc.Dims
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
  | WrongDims Dims Dims
  | IllegalExponent
  deriving (Eq)

instance Exception Error

instance Show Error where
  show (NoFunction f) = unwords ["no function:", f]
  show (ExprError e) = show e
  show (ConversionError from to) = unwords ["no conversion possible from", show from, "to", show to]
  show NoExpr = "no expression"
  show NoAnswer = "no answer"
  show WrongArity = "wrong arity"
  show (WrongDims from to) = unwords ["wrong dimensions; got", show from, "expected", show to]
  show IllegalExponent = "illegal exponent"
