module Calc.Eval where

import Calc.Conv
import Calc.Parser.Expr
import Calc.Scalar
import Calc.Units

hasPlaceholder Answer = True
hasPlaceholder (Term _) = False
hasPlaceholder (Convert x _) = hasPlaceholder x
hasPlaceholder (Unary _ _ x) = hasPlaceholder x
hasPlaceholder (Binary _ _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (BinaryConv _ _ x y) = hasPlaceholder x || hasPlaceholder y

evalExpr ans Answer = Right $ Term ans
evalExpr ans (Term x) = Right $ Term x
evalExpr ans (Convert x to) = evalConvert ans x to
evalExpr ans (Unary _ f x) = evalUnary ans f x
evalExpr ans (Binary _ f x y) = evalBinary ans f x y
evalExpr ans (BinaryConv _ f x y) = evalBinaryConv ans f x y

evalConvert ans (Term x) to = convert x to >>= evalExpr ans . Term
evalConvert ans x to = do
  x' <- evalExpr ans x
  evalConvert ans x' to

evalUnary ans f (Term x) = evalExpr ans $ Term (f x)
evalUnary ans f x = evalExpr ans x >>= evalUnary ans f

evalBinary ans f (Term x@(Scalar _ from)) (Term y@(Scalar _ to)) =
  if nullUnits to || nullUnits from
    then Right $ Term (f x y)
    else Term . (`f` y) <$> harmonize x to
evalBinary ans f x y = do
  x' <- evalExpr ans x
  y' <- evalExpr ans y
  evalBinary ans f x' y'

evalBinaryConv ans f (Term x@(Scalar _ from)) (Term y@(Scalar _ to)) =
  if nullUnits to
    then Term . (x `f`) <$> convert y from
    else Term . (`f` y) <$> convert x to
evalBinaryConv ans f x y = do
  x' <- evalExpr ans x
  y' <- evalExpr ans y
  evalBinaryConv ans f x' y'
