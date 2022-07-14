{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Scalar
import Calc.Units hiding (name)
import Data.List as L
import Data.Map as M
import Data.Maybe
import System.Console.CmdArgs
import Text.Parsec
import Text.Printf

-- command line options
data Opts = Opts
  { exprString :: String,
    dontShowUnits :: Bool,
    precision :: Maybe Int,
    vars :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

opts =
  Opts
    { exprString = def &= explicit &= name "e" &= name "expression" &= typ "EXPR",
      dontShowUnits = def &= explicit &= name "n" &= name "no-units",
      precision = def &= explicit &= name "p" &= name "precision" &= typ "INT",
      vars = def &= args &= typ "VARS"
    }
    &= summary "calc v1.0, (c) Jeffrey Massung"
    &= noAtExpand

evalVars args = sequence [evalVar x v | (x, v) <- L.zip ["x", "y", "z"] (vars args)]
  where
    evalVar x v = case runParser exprParser M.empty "" v of
      Left err -> Left $ ExprError err
      Right expr -> Right (x, expr)

run args = do
  varMap <- M.fromList <$> evalVars args
  eval varMap "" (exprString args)

outputScalar args x@(Scalar _ u)
  | nullUnits u = printf prec x
  | dontShowUnits args = printf prec x
  | otherwise = printf (prec ++ " %U") x x
  where
    prec = "%0." ++ show (fromMaybe 3 $ precision args) ++ "g"

output args (Term x) = outputScalar args x
output args _ = return ()

main = do
  args <- cmdArgs opts
  case run args of
    Left err -> print err
    Right ans -> output args ans >> putChar '\n'
