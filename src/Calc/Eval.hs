module Calc.Eval where

import Calc.Graph
import Calc.Parser

eval :: Expr -> Either String Expr
eval (Unary _ f x) = evalUnary f x
eval (Binary _ f x y) = evalBinary f x y
eval num = Right num

evalUnary f (Term x) = Right $ Term (f x)
evalUnary f x = eval x >>= evalUnary f

evalBinary f (Term x) (Term y) = Right $ Term (f x y)
evalBinary f x y = do
  x' <- eval x
  y' <- eval y
  evalBinary f x' y'

convert x from to =
  case conversionScale from to of
    Nothing -> Left "invalid type conversion"
    Just scale -> Right $ x * scale
