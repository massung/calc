{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Calc.Parser where

import Calc.Scalar
import Calc.Units as U
import Data.Functor
import Data.List as L
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

-- valid expressions
data Expr
  = Num Scalar
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr

instance Show Expr where
  show (Num x) = show x
  show _ = "<expression>"

-- unary expression operators
type UnaryOp = Scalar -> Scalar

-- binary expression operators
type BinaryOp = Scalar -> Scalar -> Scalar

calc =
  LanguageDef
    { commentStart = "",
      commentEnd = "",
      commentLine = "#",
      nestedComments = False,
      identStart = letter,
      identLetter = letter,
      opStart = oneOf "+-*/",
      opLetter = oneOf "+-*/",
      reservedNames = ["it", "to", "as"],
      reservedOpNames = ["+", "-", "*", "/"],
      caseSensitive = True
    }

lexer = makeTokenParser calc

parseExpr = parse (ws >> expr) ""
  where
    ws = whiteSpace lexer

expr :: Parsec String () Expr
expr = buildExpressionParser table term

term = parens lexer expr <|> number <|> singleUnit

number = do
  n <- naturalOrFloat lexer
  u <- units
  return $ case n of
    Left i -> Num (Scalar (fromIntegral i) u)
    Right f -> Num (Scalar f u)

units = do
  us <- optionMaybe $ many1 unit
  return $ us <&> U.fromList . L.map (, 1 :: Int)

unit = identifier lexer <&> Unit

singleUnit = unit <&> Num . Scalar 1 . Just . U.singleton

table =
  [ [prefix "-" negate, prefix "+" id],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
  ]

binary name f = Infix (do reservedOp lexer name; return $ Binary f)

prefix name f = Prefix (do reservedOp lexer name; return $ Unary f)
