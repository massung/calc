{-# LANGUAGE OverloadedStrings #-}

module Calc.Defs where

import Calc.Error
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units hiding (_pi)
import Control.Monad
import Data.Map.Strict as M

data Def = Def ([Scalar] -> Either Error Scalar) [Units]

defMap =
  M.fromList
    [ ("pi", Def _pi []),
      ("sin", Def _sin ["rad"]),
      ("cos", Def _cos ["rad"])
    ]

apply (Def func units) args = case zipWithM convert args units of
  Right xs -> func [fromRational x | (Scalar x _ _) <- xs]
  Left e -> Left e

_pi [] = Right $ scalar pi "rad"
_pi _ = Left WrongArity

_sin [x] = Right $ scalar (sin $ fromRational $ toRational x) mempty
_sin _ = Left WrongArity

_cos [x] = Right $ scalar (cos $ fromRational $ toRational x) mempty
_cos _ = Left WrongArity
