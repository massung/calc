module Calc.Eval where

import Calc.Conv
import Calc.Graph
import Calc.Parser.Expr
import Calc.Scalar

eval :: Expr -> Either String Expr
eval (Term x) = Right $ Term x
eval (Convert x to) = evalConvert x to
eval (Unary _ f x) = evalUnary f x
eval (Binary _ f x y) = evalBinary f x y

evalConvert (Term x) to = convert x to >>= eval . Term
evalConvert x to = do
  x' <- eval x
  evalConvert x' to

evalUnary f (Term x) = eval $ Term (f x)
evalUnary f x = eval x >>= evalUnary f

evalBinary f (Term x) (Term y@(Scalar _ u)) = Term . (`f` y) <$> convert x u
evalBinary f x y = do
  x' <- eval x
  y' <- eval y
  evalBinary f x' y'
