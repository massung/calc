{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Scalar
import Calc.Scalar
import Calc.Units hiding (name)
import Control.Monad
import Data.Either.Extra
import Data.Maybe
import System.Console.CmdArgs
import System.IO
import System.IO.Unsafe
import Text.Parsec
import Text.Printf

-- command line options
data Opts = Opts
  { exprString :: Maybe String,
    dontShowUnits :: Bool,
    interactive :: Bool,
    precision :: Maybe Int,
    ans :: [String]
  }
  deriving (Data, Typeable, Show, Eq)

opts =
  Opts
    { exprString = def &= explicit &= name "e" &= name "expression" &= typ "EXPR",
      dontShowUnits = def &= explicit &= name "n" &= name "no-units",
      interactive = def &= explicit &= name "i" &= name "interactive",
      precision = def &= explicit &= name "p" &= name "precision" &= typ "INT",
      ans = def &= args &= typ "EXPR"
    }
    &= summary "calc v1.0, (c) Jeffrey Massung"
    &= noAtExpand

getAns :: Opts -> Either Error Scalar
getAns args = case ans args of
  [] -> Right 0
  xs -> mapLeft ExprError . parse scalarParser "" $ unwords xs

outputScalar args x@(Scalar _ u)
  | nullUnits u = printf prec x
  | dontShowUnits args = printf prec x
  | otherwise = printf (prec ++ " %U") x x
  where
    prec = "%0." ++ show (fromMaybe 3 $ precision args) ++ "g"

prompt = do
  putStr ">> "
  hFlush stdout
  getLine

runInteractive :: Opts -> Scalar -> IO ()
runInteractive args ans = do
  expr <- prompt
  putStr "== "
  case runExpr args {exprString = Just expr} ans of
    Left err -> print err >> runInteractive args ans
    Right ans' -> do
      outputScalar args ans'
      putChar '\n'
      runInteractive args ans'

runExpr :: Opts -> Scalar -> Either Error Scalar
runExpr args ans =
  case exprString args of
    Nothing -> Right ans
    Just expr -> eval ans "" expr

run :: Opts -> Either Error Scalar
run args = do
  ans <- getAns args
  ans' <- runExpr args ans
  if interactive args
    then const (Right ans') $! unsafePerformIO (runInteractive args ans')
    else return ans'

main :: IO ()
main = do
  args <- cmdArgs opts

  -- show an error or answer
  either print (outputScalar args) $ run args
