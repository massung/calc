{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Scalar
import Calc.Scalar
import Calc.Units hiding (name)
import Control.Exception
import Data.Either.Extra
import Data.List as L
import Data.Maybe
import System.Console.CmdArgs
import System.IO
import System.IO.Unsafe
import Text.Parsec hiding ((<|>), try)
import Text.Printf

-- command line options
data Opts = Opts
  { exprString :: String,
    precision :: Maybe Int,
    dontShowUnits :: Bool,
    interactive :: Bool
  }
  deriving (Data, Typeable, Show, Eq)

opts =
  Opts
    { exprString = def &= argPos 0 &= typ "EXPRESSION",
      dontShowUnits = def &= explicit &= name "n" &= name "no-units",
      interactive = def &= explicit &= name "i" &= name "interactive",
      precision = def &= explicit &= name "p" &= name "precision" &= typ "INT"
    }
    &= summary "calc v1.0, (c) Jeffrey Massung"
    &= noAtExpand

outputScalar args x@(Scalar _ u)
  | nullUnits u = printf prec x
  | dontShowUnits args = printf prec x
  | otherwise = printf (prec ++ " %U") x x
  where
    prec = "%0." ++ show (fromMaybe 3 $ precision args) ++ "g"

prompt :: IO String
prompt = do
  putStr ">> "
  hFlush stdout
  getLine

output :: Opts -> Scalar -> IO ()
output args ans = do
  outputScalar args ans
  putChar '\n'

parseExpr :: String -> IO Expr
parseExpr s = case mapLeft ExprError $ parse exprParser "" s of
  Right expr -> return expr
  Left err -> throw err

parseInput :: String -> IO Scalar
parseInput s = case mapLeft ExprError $ parse scalarParser "" s of
  Right x -> return x
  Left err -> throw err

runExpr :: Expr -> Scalar -> IO Scalar
runExpr expr ans = case evalExpr ans expr of
  Right (Term x) -> return x
  Right _ -> return 0  -- unreachable
  Left err -> throw err

runInteractive :: Opts -> Scalar -> IO ()
runInteractive args ans = do
  expr <- prompt >>= parseExpr
  ans' <- runExpr expr ans
  putStr "== "
  output args ans'
  runInteractive args ans'

runOnce :: Opts -> Expr -> String -> IO ()
runOnce args expr s = do
  ans <- parseInput s
  ans' <- runExpr expr ans
  output args ans'

run :: Opts -> Expr -> IO ()
run args expr = do
  s <- getLine
  if null s
    then run args expr
    else do
      runOnce args expr s
      run args expr

main :: IO ()
main = do
  args <- cmdArgs opts
  if interactive args
    then runInteractive args 0
    else do
      expr <- parseExpr $ exprString args
      if not $ hasPlaceholder expr
        then runExpr expr 0 >>= output args
        else run args expr
                `catch` \(ex :: IOException) -> return ()
                `catch` \(ex :: Error) -> print ex
