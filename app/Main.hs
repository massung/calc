{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calc.Eval
import Calc.Expr
import Calc.Units
import Data.Either.Extra
import System.Console.CmdArgs

-- command line options
data Opts = Opts
  { outputUnits :: Maybe String,
    exprString :: String
  }
  deriving (Data, Typeable, Show, Eq)

opts =
  Opts
    { exprString = def &= name "e" &= typ "EXPR" &= help "Expression",
      outputUnits = def &= name "o" &= typ "UNITS" &= help "Output units"
    }
    &= summary "calc v1.0, (c) Jeffrey Massung"

evalExpr args = mapLeft show (parseExpr $ exprString args) >>= eval

run args = case evalExpr args of
  Right result -> print result
  Left err -> print err

main = cmdArgs opts >>= run
