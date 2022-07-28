{-# LANGUAGE OverloadedStrings #-}

module Calc.Defs where

import Calc.Error
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units hiding (_pi)
import Control.Monad
import Data.Map.Strict as M

data Def = Def ([Scalar] -> Either Error Scalar) [Arg]

data Arg = Any | Typed Units

defMap =
  M.fromList
    [ ("sqrt", Def _sqrt [Any]),
      ("pi", Def _pi []),
      ("sin", Def _sin [Typed "rad"]),
      ("cos", Def _cos [Typed "rad"])
    ]

mapArgs = zipWithM mapArg
  where
    mapArg x Any = Right x
    mapArg x (Typed u) = convert x u

apply (Def func args) xs = case mapArgs xs args of
  Right xs -> func xs
  Left e -> Left e

_sqrt [x] = powScalar x 0.5
_sqrt _ = Left WrongArity

_pi [] = Right $ scalar pi "rad"
_pi _ = Left WrongArity

_sin [x] = Right $ scalar (sin $ fromRational $ toRational x) mempty
_sin _ = Left WrongArity

_cos [x] = Right $ scalar (cos $ fromRational $ toRational x) mempty
_cos _ = Left WrongArity
