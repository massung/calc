module Calc.Eval where

import Calc.Graph
import Calc.Parser
import Calc.Scalar
import Calc.Units
import Data.Map

eval :: Expr -> Either String Expr
eval (Unary _ f x) = evalUnary f x
eval (Binary _ f x y) = evalBinary f x y
eval (Term x) = Right . Term $ simplify x

evalUnary f (Term x) = eval $ Term (f x)
evalUnary f x = eval x >>= evalUnary f

evalBinary f (Term x) (Term y) = eval $ Term (f x y)
evalBinary f x y = do
  x' <- eval x
  y' <- eval y
  evalBinary f x' y'

simplify s@(Scalar n (Just (Units u))) =
  case findConversion $ combinations (keys u) of
    Nothing -> s
    Just n -> simplify (s * n)
  where
    combinations xs = [(x, y) | x <- xs, y <- xs, x /= y]

    -- attempt to find a possible unit conversion
    findConversion [] = Nothing
    findConversion ((from, to) : rest) =
      case conversionScale from to of
        Nothing -> findConversion rest
        x -> x
simplify s = s
