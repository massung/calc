{-# LANGUAGE OverloadedStrings #-}

module Calc.Def where

import Calc.Error
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units hiding (_pi)
import Control.Monad
import Data.Functor
import Data.Map.Strict as M
import Data.Maybe
import Data.String

data Def = Def ([Double] -> Either Error Scalar) [Units]

instance IsString Def where
  fromString = fromMaybe (error "no parse") . (defMap !?)

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

_sin [x] = Right $ scalar (sin x) mempty
_sin _ = Left WrongArity

_cos [x] = Right $ scalar (cos x) mempty
_cos _ = Left WrongArity
