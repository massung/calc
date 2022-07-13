{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Data.Either.Extra
import Data.List as L
import Data.Map as M
import System.Console.CmdArgs
import Text.Parsec

-- command line options
data Opts = Opts
  { outputUnits :: Maybe String,
    exprString :: String,
    vars :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

opts =
  Opts
    { exprString = def &= name "e" &= typ "EXPR" &= help "Expression",
      outputUnits = def &= name "o" &= typ "UNITS" &= help "Output units",
      vars = def &= args &= typ "VARS"
    }
    &= summary "calc v1.0, (c) Jeffrey Massung"

evalVars args = sequence [evalVar x v | (x, v) <- L.zip ["x", "y", "z"] (vars args)]
  where
    evalVar x v = case runParser exprParser M.empty "" v of
      Left err -> Left $ ExprError err
      Right expr -> Right (x, expr)

run args = either show show result
  where
    result = do
      varMap <- M.fromList <$> evalVars args
      eval varMap "" (exprString args)

main = cmdArgs opts >>= print . run
