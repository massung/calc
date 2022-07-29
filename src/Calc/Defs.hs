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

data Arg = None | Any | Typed Dims

defMap =
  M.fromList
    [ ("sqrt", Def _sqrt [Any]),
      ("log", Def _log [None]),
      ("exp", Def _exp [None]),
      ("pi", Def _pi []),
      ("sin", Def _sin $ dims [Angle]),
      ("cos", Def _cos $ dims [Angle]),
      ("tan", Def _tan $ dims [Angle]),
      ("asin", Def _asin [None]),
      ("acos", Def _acos [None]),
      ("atan", Def _atan [None])
    ]
  where
    dims xs = [Typed $ baseDims x | x <- xs]

mapArgs = zipWithM mapArg
  where
    mapArg x Any = Right x
    mapArg x@(Scalar _ d _) None =
      if nullDims d
        then Right x
        else Left $ WrongDims d mempty
    mapArg x@(Scalar _ d _) (Typed dims) =
      if nullDims d || d == dims
        then Right x
        else Left $ WrongDims d dims

apply (Def func args) xs = case mapArgs xs args of
  Right xs -> func xs
  Left e -> Left e

unaryDef f = fmap $ fromReal . f . fromRational . toRational

_sqrt [x] = powScalar x 0.5
_sqrt _ = Left WrongArity

_log [x] = unaryDef log $ Right x
_log _ = Left WrongArity

_exp [x] = unaryDef exp $ Right x
_exp _ = Left WrongArity

_pi [] = Right $ fromReal pi
_pi _ = Left WrongArity

_sin [x] = unaryDef sin $ convert x "rad"
_sin _ = Left WrongArity

_cos [x] = unaryDef cos $ convert x "rad"
_cos _ = Left WrongArity

_tan [x] = unaryDef tan $ convert x "rad"
_tan _ = Left WrongArity

_asin [x] = unaryDef asin (Right x) >>= (`convert` "rad")
_asin _ = Left WrongArity

_acos [x] = unaryDef acos (Right x) >>= (`convert` "rad")
_acos _ = Left WrongArity

_atan [x] = unaryDef atan (Right x) >>= (`convert` "rad")
_atan _ = Left WrongArity
