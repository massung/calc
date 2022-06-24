module Calc.Eval where

import Calc.Graph
import Calc.Parser

eval :: Expr -> Either String Expr
eval (Term x) = Right $ Term x
eval (Unary _ f x) = evalUnary f x
eval (Binary _ f x y) = evalBinary f x y
eval (Convert x to) = evalConvert x to

evalUnary f (Term x) = eval $ Term (f x)
evalUnary f x = eval x >>= evalUnary f

evalBinary f (Term x) (Term y) = eval $ Term (f x y)
evalBinary f x y = do
  x' <- eval x
  y' <- eval y
  evalBinary f x' y'

evalConvert (Term x) to = convertScalar x to >>= eval . Term
evalConvert x to = do
  x' <- eval x
  evalConvert x' to
