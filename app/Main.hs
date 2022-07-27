{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Calc.Defs
import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Scalar
import Calc.Script
import Calc.Units hiding (name)
import Control.Applicative
import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either.Extra
import Data.List.Extra as L
import Data.Map.Strict as M
import Data.Maybe
import System.Console.CmdArgs
import System.Environment
import System.IO
import Text.Parsec hiding (try, (<|>))
import Text.Parsec.Token
import Text.Printf

-- command line options
data Opts = Opts
  { scriptFiles :: [String],
    precision :: Maybe Int,
    delim :: Maybe String,
    noUnits :: Bool,
    exprStrings :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

motd = printf "calc v%d.%d.%d, (c) Jeffrey Massung" major minor patch
  where
    major = 1 :: Int
    minor = 0 :: Int
    patch = 0 :: Int

getOpts =
  cmdArgs $
    Opts
      { scriptFiles = def &= explicit &= name "s" &= name "script" &= typ "FILE",
        precision = def &= explicit &= name "p" &= name "precision" &= typ "DIGITS",
        delim = def &= explicit &= name "d" &= name "delim" &= typ "FS",
        noUnits = def &= explicit &= name "n" &= name "no-units",
        exprStrings = def &= args &= typ "EXPRESSION [ARGS...]"
      }
      &= program "calc"
      &= summary motd
      &= details
        [ "Examples:",
          "  calc '1+2'",
          "  calc '6 ft + 3 in : m'",
          "  calc '500 N * 10 ft to BTU'",
          "  calc '10 GB / 2 hr to MB/s'",
          "  calc '30 W * 6 min to J'",
          "  calc '2 * [sin 45 deg]'",
          "  calc '100 hz * _ m : mph' < values.txt"
        ]
      &= noAtExpand

printAns :: Opts -> Scalar -> IO Scalar
printAns opts x@(Scalar _ d u) =
  if nullUnits u || noUnits opts
    then printf (prec ++ "\n") x >> return x
    else printf (prec ++ " %U\n") x x >> return x
  where
    prec = "%0." ++ show (fromMaybe 2 $ precision opts) ++ "g"

parseExpr :: Map String Def -> String -> IO Expr
parseExpr defs s = either throw return $ mapLeft ExprError $ runParser parser defs "" s
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
  fs <- lookupEnv "FS"
  parseInputs $ splitOn (fromMaybe "," $ delim opts <|> fs) input

prompt :: Map String Def -> IO Expr
prompt defs = do
  putStr ">> "
  hFlush stdout
  s <- getLine
  if L.null s
    then prompt defs
    else parseExpr defs s

runExpr :: Opts -> Expr -> [Scalar] -> IO Scalar
runExpr opts expr xs = either throw (printAns opts) result
  where
    result = evalState (runExceptT $ evalExpr expr) xs

runEval :: Opts -> Map String Def -> [Scalar] -> IO Scalar
runEval opts defs xs = do
  expr <- prompt defs
  putStr "== "
  runExpr opts expr xs

runInteractive :: Opts -> Map String Def -> [Scalar] -> IO ()
runInteractive opts defs xs = do
  repl
    `catches` [ Handler $ \(ex :: IOException) -> return (),
                Handler $ \(ex :: Error) -> print ex >> runInteractive opts defs xs
              ]
  where
    repl = runEval opts defs xs >>= runInteractive opts defs . (: xs)

runLoop :: Opts -> Expr -> IO ()
runLoop opts expr = do
  inputs <- parseCsvInputs opts
  runExpr opts expr inputs
  runLoop opts expr

run :: Opts -> Map String Def -> [String] -> IO ()
run opts defs [] = putStrLn motd >> runInteractive opts defs []
run opts defs (exprString : inputs) = do
  (expr, xs) <- (,) <$> parseExpr defs exprString <*> parseInputs inputs

  -- no placeholder (run once), no inputs (use stdin), or run once w/ CLI args
  if
      | not (hasPlaceholder expr) -> void $ runExpr opts expr []
      | L.null xs -> runLoop opts expr
      | otherwise -> void $ runExpr opts expr xs

main :: IO ()
main = do
  opts <- getOpts

  -- load all the scripts to create a single defs map
  defs <- loadScripts defMap $ scriptFiles opts

  -- handle EOF or expression error
  run opts defs (exprStrings opts)
    `catches` [ Handler $ \(ex :: IOException) -> return (),
                Handler $ \(ex :: Error) -> print ex
              ]
