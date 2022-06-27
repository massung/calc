module Calc.Eval where

import Calc.Expr
import Calc.Graph
import Calc.Scalar

eval :: Expr -> Either String Expr
eval (Term x) = Right $ Term x
eval (Convert x to) = Right x -- evalConvert x to
eval (Unary _ f x) = evalUnary f x
eval (Binary _ f x y) = evalBinary f x y
eval (BinaryConvert _ f x y) = Right x -- evalBinaryConvert f x y

-- evalConvert (Term x) to = convertScalar x to >>= eval . Term
-- evalConvert x to = do
--   x' <- eval x
--   evalConvert x' to

evalUnary f (Term x) = eval $ Term (f x)
evalUnary f x = eval x >>= evalUnary f

evalBinary f (Term x) (Term y) = eval $ Term (f x y)
evalBinary f x y = do
  x' <- eval x
  y' <- eval y
  evalBinary f x' y'

-- evalBinaryConvert f (Term x) (Term y@(Scalar _ Nothing)) = eval $ Term (f x y)
-- evalBinaryConvert f (Term x) (Term y@(Scalar _ (Just u))) = do
--   x' <- convertScalar x u
--   eval $ Term (f x' y)
-- evalBinaryConvert f x y = do
--   x' <- eval x
--   y' <- eval y
--   evalBinaryConvert f x' y'
