module Calc.Eval where

import Calc.Conv
import Calc.Error
import Calc.Parser.Expr
import Calc.Scalar
import Calc.Units
import Control.Monad.Except
import Control.Monad.State.Strict

type Eval = ExceptT Error (State [Scalar])

evalExpr :: Expr -> Eval Scalar
evalExpr Answer = evalAnswer
evalExpr (Term x) = return x
evalExpr (Convert to x) = evalConvert x to
evalExpr (Unary _ f x) = evalUnary f x
evalExpr (Binary _ f x y) = evalBinary f x y
evalExpr (BinaryConv _ f x y) = evalBinaryConv f x y

evalAnswer :: Eval Scalar
evalAnswer = do
  st <- lift get
  case st of
    [] -> throwError NoAnswer
    (x : xs) -> do
      put xs
      return x

evalConvert :: Expr -> Units -> Eval Scalar
evalConvert (Term x) to = either throwError return $ convert x to
evalConvert x to = do
  x' <- evalExpr x
  either throwError return $ convert x' to

evalUnary :: (Scalar -> Scalar) -> Expr -> Eval Scalar
evalUnary f (Term x) = return $ f x
evalUnary f x = do
  x' <- evalExpr x
  return $ f x'

evalBinary :: (Scalar -> Scalar -> Scalar) -> Expr -> Expr -> Eval Scalar
evalBinary f x y = do
  x'@(Scalar _ from) <- evalExpr x
  y'@(Scalar _ to) <- evalExpr y
  if nullUnits from || nullUnits to
    then return $ f x' y'
    else case (`f` y') <$> convert x' to of
      Right ans -> return ans
      Left _ -> return $ f x' y'

evalBinaryConv :: (Scalar -> Scalar -> Scalar) -> Expr -> Expr -> Eval Scalar
evalBinaryConv f x y = do
  x'@(Scalar _ from) <- evalExpr x
  y'@(Scalar _ to) <- evalExpr y
  if nullUnits to
    then either throwError (return . (x' `f`)) $ convert y' from
    else either throwError (return . (`f` y')) $ convert x' to
