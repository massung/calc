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
import Data.Ratio
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
    sciNotation :: Bool,
    delim :: Maybe String,
    noUnits :: Bool,
    exprStrings :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

motd = printf "Tournesol v%d.%d.%d, (c) Jeffrey Massung" major minor patch
  where
    major = 0 :: Int
    minor = 9 :: Int
    patch = 0 :: Int

getOpts =
  cmdArgs $
    Opts
      { scriptFiles = def &= explicit &= name "f" &= name "functions" &= typ "FILE" &= help "Load Tournesol script functions file",
        precision = def &= explicit &= name "p" &= name "precision" &= typ "DIGITS" &= help "Precision digits to output, defaults to 2",
        sciNotation = def &= explicit &= name "g" &= name "sci-notation" &= help "Output using scientific notation",
        noUnits = def &= explicit &= name "n" &= name "no-units" &= help "Don't output units",
        delim = def &= explicit &= name "d" &= name "delimiter" &= typ "FS" &= help "Input stream delimiter, defaults to $FS",
        exprStrings = def &= args &= typ "EXPRESSION [ARGS...]"
      }
      &= program "Tournesol"
      &= summary motd
      &= details
        [ "Examples:",
          "  tn '1+2'",
          "  tn '6 ft + 3 in : m'",
          "  tn '500 N * 10 ft to BTU'",
          "  tn '10 GB / 2 hr to MB/s'",
          "  tn '2 * (1500 cm)^2 to acre'",
          "  tn '30 W * 6 min to J'",
          "  tn '2 * [sin 45 deg]'",
          "  tn '100 hz * _ m : mph' < values.txt"
        ]
      &= noAtExpand

printFormat :: Opts -> Scalar -> String
printFormat opts (Scalar x _ _) = "%0." ++ prec ++ (if sciNotation opts then "g" else "f")
  where
    prec =
      if denominator x == 1
        then "0"
        else show (fromMaybe 2 $ precision opts)

printAns :: Opts -> Scalar -> IO Scalar
printAns opts x@(Scalar _ d u) =
  if nullUnits u || noUnits opts
    then printf (printFormat opts x ++ "\n") x >> return x
    else printf (printFormat opts x ++ " %U\n") x x >> return x

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
    repl = runEval opts defs xs >>= runInteractive opts defs . L.take 5 . (: xs)

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
  defs <- builtInDefs >>= (`loadScripts` scriptFiles opts)

  -- handle EOF or expression error
  run opts defs (exprStrings opts)
    `catches` [ Handler $ \(ex :: IOException) -> return (),
                Handler $ \(ex :: Error) -> print ex
              ]
