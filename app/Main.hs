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
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either.Extra
import Data.Maybe
import System.Console.CmdArgs
import System.IO
import Text.Parsec hiding (try, (<|>))
import Text.Printf

-- command line options
data Opts = Opts
  { exprStrings :: [String],
    precision :: Maybe Int,
    dontShowUnits :: Bool
  }
  deriving (Data, Typeable, Show, Eq)

opts =
  Opts
    { exprStrings = def &= args &= typ "EXPRESSION",
      dontShowUnits = def &= explicit &= name "n" &= name "no-units",
      precision = def &= explicit &= name "p" &= name "precision" &= typ "INT"
    }
    &= summary "calc v1.0, (c) Jeffrey Massung"
    &= noAtExpand

outputScalar args x@(Scalar _ u)
  | nullUnits u = printf prec x
  | dontShowUnits args = printf prec x
  | otherwise = printf (prec ++ " %U") x x
  where
    prec = "%0." ++ show (fromMaybe 2 $ precision args) ++ "g"

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

runExpr :: Expr -> [Scalar] -> IO Scalar
runExpr expr ans = case runState (runExceptT $ evalExpr expr) ans of
  (Right x, _) -> return x
  (Left e, _) -> throw e

runInteractive :: Opts -> [Scalar] -> IO ()
runInteractive args ans = do
  expr <- prompt >>= parseExpr
  ans' <- runExpr expr ans
  putStr "== "
  output args ans'
  runInteractive args ans'

runOnce :: Opts -> Expr -> [Scalar] -> IO ()
runOnce args expr s = do
  ans <- parseInput s
  ans' <- runExpr expr ans
  output args ans'

run :: Opts -> Expr -> [Scalar] -> IO ()
run args expr [] = do
  s <- getLine
  if null s
    then run args expr
    else do
      runOnce args expr s
      run args expr
run args expr ans = do

main :: IO ()
main = do
  args <- cmdArgs opts
  case exprStrings args of
    [] -> runInteractive args []
    (exprString : answers) -> do
      expr <- parseExpr exprString
      if not $ hasPlaceholder expr
        then runExpr expr [] >>= output args
        else do
          case sequence [parse scalarParser "" s | s <- answers] of
            Left err -> throw err
            Right ans -> run args expr ans
            `catches` [ Handler $ \(ex :: IOException) -> return (),
                        Handler $ \(ex :: Error) -> print ex
                      ]
