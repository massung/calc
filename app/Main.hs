{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Scalar
import Calc.Units hiding (name)
import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either.Extra
import Data.List.Extra
import Data.Maybe
import System.Console.CmdArgs
import System.IO
import Text.Parsec hiding (try, (<|>))
import Text.Parsec.Token
import Text.Printf

-- command line options
data Opts = Opts
  { precision :: Maybe Int,
    noUnits :: Bool,
    delim :: Maybe String,
    exprStrings :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

getOpts =
  cmdArgs $
    Opts
      { noUnits = def &= explicit &= name "n" &= name "no-units",
        precision = def &= explicit &= name "p" &= name "precision" &= typ "N",
        delim = def &= explicit &= name "d" &= name "delimiter" &= typ "SEP",
        exprStrings = def &= args &= typ "EXPRESSION [ARGS...]"
      }
      &= program "calc"
      &= summary "calc v1.0, (c) Jeffrey Massung"
      &= details ["Scalar expression and units calculator"]
      &= noAtExpand

printAns :: Opts -> Scalar -> IO Scalar
printAns opts x@(Scalar _ d u) =
  if nullUnits u || noUnits opts
    then printf (prec ++ "\n") x >> return x
    else printf (prec ++ " %U\n") x x >> return x
  where
    prec = "%0." ++ show (fromMaybe 2 $ precision opts) ++ "g"

parseExpr :: String -> IO Expr
parseExpr s = either throw return $ mapLeft ExprError $ parse parser "" s
  where
    parser = do
      whiteSpace lexer
      expr <- exprParser
      eof
      return expr

parseInputs :: [String] -> IO [Scalar]
parseInputs inputs = either (throw . ExprError) return $ sequence xs
  where
    xs = [parseScalar $ trimStart s | s <- inputs]

parseCsvInputs :: Opts -> IO [Scalar]
parseCsvInputs opts = do
  input <- getLine
  parseInputs $ splitOn (fromMaybe "," $ delim opts) input

prompt :: IO Expr
prompt = do
  putStr ">> "
  hFlush stdout
  s <- getLine
  if null s
    then prompt
    else parseExpr s

runExpr :: Opts -> Expr -> [Scalar] -> IO Scalar
runExpr opts expr xs = either throw (printAns opts) result
  where
    result = evalState (runExceptT $ evalExpr expr) xs

runEval :: Opts -> [Scalar] -> IO Scalar
runEval opts xs = do
  expr <- prompt
  putStr "== "
  runExpr opts expr xs

runInteractive :: Opts -> [Scalar] -> IO ()
runInteractive opts xs = do
  repl
    `catches` [ Handler $ \(ex :: IOException) -> return (),
                Handler $ \(ex :: Error) -> print ex >> runInteractive opts xs
              ]
  where
    repl = runEval opts xs >>= runInteractive opts . (: xs)

runLoop :: Opts -> Expr -> IO ()
runLoop opts expr = do
  inputs <- parseCsvInputs opts
  runExpr opts expr inputs
  runLoop opts expr

run :: Opts -> [String] -> IO ()
run opts [] = runInteractive opts []
run opts (exprString : inputs) = do
  (expr, xs) <- (,) <$> parseExpr exprString <*> parseInputs inputs

  -- no placeholder (run once), no inputs (use stdin), or run once w/ CLI args
  if
      | not (hasPlaceholder expr) -> void $ runExpr opts expr []
      | null xs -> runLoop opts expr
      | otherwise -> void $ runExpr opts expr xs

main :: IO ()
main = do
  opts <- getOpts

  -- handle EOF or expression error
  run opts (exprStrings opts)
    `catches` [ Handler $ \(ex :: IOException) -> return (),
                Handler $ \(ex :: Error) -> print ex
              ]
