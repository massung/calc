{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
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
  { precision :: Maybe Int,
    dontShowUnits :: Bool,
    exprStrings :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

getOpts =
  cmdArgs $ Opts
    { dontShowUnits = def &= explicit &= name "n" &= name "no-units",
      precision = def &= explicit &= name "p" &= name "precision" &= typ "INT",
      exprStrings = def &= args &= typ "EXPRESSION"
    }
    &= summary "calc v1.0, (c) Jeffrey Massung"
    &= noAtExpand

printAns :: Opts -> Scalar -> IO Scalar
printAns opts x@(Scalar _ u) =
  if nullUnits u || dontShowUnits opts
    then printf (prec ++ "\n") x >> return x
    else printf (prec ++ "%U\n") x x >> return x
  where
    prec = "%0." ++ show (fromMaybe 2 $ precision opts) ++ "g"

parseExpr :: String -> IO Expr
parseExpr s = case mapLeft ExprError $ parse exprParser "" s of
  Right expr -> return expr
  Left err -> throw err

parseInputs :: [String] -> IO [Scalar]
parseInputs inputs = either (throw . ExprError) return $ sequence xs
  where
    xs = [parse scalarParser "" s | s <- inputs]

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

runInteractive :: Opts -> [Scalar] -> IO ()
runInteractive opts xs = do
  expr <- prompt
  putStr "== "
  ans' <- runExpr opts expr xs
  runInteractive opts (ans' : xs)

runLoop :: Opts -> Expr -> IO ()
runLoop opts expr = do
  s <- getLine
  inputs <- parseInputs [s]
  runExpr opts expr inputs
  runLoop opts expr

run :: Opts -> [String] -> IO ()
run opts [] = runInteractive opts []
run opts (exprString : inputs) = do
  (expr, xs) <- (,) <$> parseExpr exprString <*> parseInputs inputs

  -- no placeholder, no inputs, or run once
  if | not (hasPlaceholder expr) -> void $ runExpr opts expr []
     | null xs -> runLoop opts expr
     | otherwise -> void $ runExpr opts expr xs

main :: IO ()
main = do
  opts <- getOpts

  -- handle EOF or expression error
  run opts (exprStrings opts)
    `catches`
      [ Handler $ \(ex :: IOException) -> return (),
        Handler $ \(ex :: Error) -> print ex
      ]
