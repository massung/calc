{-# LANGUAGE MultiWayIf #-}

module Calc.Eval where

import Calc.Conv
import Calc.Error
import Calc.Parser.Expr
import Calc.Scalar
import Calc.Units
import Data.Either.Extra
import Text.Parsec

eval ans source s = do
  expr <- mapLeft ExprError (runParser exprParser ans source s)
  term <- evalExpr expr
  case term of
    Term ans' -> return ans'
    _ -> error "Unreachable!"

evalExpr (Term x) = Right $ Term x
evalExpr (Convert x to) = evalConvert x to
evalExpr (Unary _ f x) = evalUnary f x
evalExpr (Binary _ f x y) = evalBinary f x y
evalExpr (BinaryConv _ f x y) = evalBinaryConv f x y

evalConvert (Term x) to = convert x to >>= evalExpr . Term
evalConvert x to = do
  x' <- evalExpr x
  evalConvert x' to

evalUnary f (Term x) = evalExpr $ Term (f x)
evalUnary f x = evalExpr x >>= evalUnary f

evalBinary f (Term x@(Scalar _ from)) (Term y@(Scalar _ to)) =
  if nullUnits to || nullUnits from
    then Right $ Term (f x y)
    else Term . (`f` y) <$> harmonize x to
evalBinary f x y = do
  x' <- evalExpr x
  y' <- evalExpr y
  evalBinary f x' y'

evalBinaryConv f (Term x@(Scalar _ from)) (Term y@(Scalar _ to)) =
  if nullUnits to
    then Term . (x `f`) <$> convert y from
    else Term . (`f` y) <$> convert x to
evalBinaryConv f x y = do
  x' <- evalExpr x
  y' <- evalExpr y
  evalBinaryConv f x' y'
