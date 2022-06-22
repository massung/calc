module Calc.Eval where

import Calc.Graph
import Calc.Parser

eval (Unary f x) = evalUnary f x
eval (Binary f x y) = evalBinary f x y
eval num = Right num

evalUnary f (Num x u) = Right $ Num (- x) u
evalUnary f x = eval x >>= evalUnary f

evalBinary f (Num x Nothing) (Num y (Just u)) = Right $ Num (f x y) (Just u)
evalBinary f (Num x (Just u)) (Num y Nothing) = Right $ Num (f x y) (Just u)
evalBinary f (Num x (Just u1)) (Num y (Just u2)) =
  if u1 == u2
    then Right $ Num (f x y) (Just u1)
    else do
      y' <- convert y u2 u1
      return $ Num (f x y') (Just u1)
evalBinary f x y = do
  x' <- eval x
  y' <- eval y
  evalBinary f x' y'

convert x from to =
  case conversionScale from to of
    Nothing -> Left "invalid type conversion"
    Just scale -> Right $ x * scale
