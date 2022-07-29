{-# LANGUAGE OverloadedStrings #-}

module Calc.Defs where

import Calc.Dims
import Calc.Error
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units hiding (_pi)
import Control.Monad
import Data.Map.Strict as M

data Def = Def ([Scalar] -> Either Error Scalar) [Arg]

data Arg = Any | Typed Dims

defMap =
  M.fromList
    [ ("sqrt", Def _sqrt [Any]),
      ("pi", Def _pi []),
      ("sin", Def _sin [Typed $ baseDims Angle]),
      ("cos", Def _cos [Typed $ baseDims Angle])
    ]

mapArgs = zipWithM mapArg
  where
    mapArg x Any = Right x
    mapArg x@(Scalar _ d _) (Typed dims) =
      if nullDims d || d == dims
        then Right x
        else Left $ WrongDims d dims

apply (Def func args) xs = case mapArgs xs args of
  Right xs -> func xs
  Left e -> Left e

_sqrt [x] = powScalar x 0.5
_sqrt _ = Left WrongArity

_pi [] = Right $ fromReal pi
_pi _ = Left WrongArity

_sin [x] = fromReal . sin . fromRational . toRational <$> convert x "rad"
_sin _ = Left WrongArity

_cos [x] = fromReal . cos . fromRational . toRational <$> convert x "rad"
_cos _ = Left WrongArity
