{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Calc.Eval
import Calc.Parser
import System.Console.CmdArgs
import System.Environment

-- command line options
newtype Opts = Opts
  { evalExpr :: String
  }
  deriving (Data, Typeable, Show, Eq)

opts =
  Opts {evalExpr = def &= name "e" &= help "Expression to evaluate"}
    &= verbosity
    &= summary "calc v0.1.0, (c) Jeffrey Massung"

main = do
  args <- cmdArgs opts
  case parseExpr (evalExpr args) of
    Right expr -> print $ eval expr
    Left err -> print err
