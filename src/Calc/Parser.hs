{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser where

import Data.Foldable
import Data.Functor
import Data.Scientific
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

-- valid expressions
data Expr
  = Num Scientific [String]
  | Unary UnaryOp Expr
  | BinaryConv BinaryOp Expr Expr -- units converted
  | BinaryComp BinaryOp Expr Expr -- units combined

instance Show Expr where
  show (Num x []) = show x
  show (Num x xs) = show x ++ " " ++ concat xs
  show _ = "<expression>"

-- Func String [Expr]
-- Conv Expr String

-- unary expression operators
type UnaryOp = Scientific -> Scientific

-- binary expression operators
type BinaryOp = Scientific -> Scientific -> Scientific

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

expr :: Parsec String () Expr
expr = buildExpressionParser table term

term = parens lexer expr <|> number <|> singleUnit

number = do
  n <- naturalOrFloat lexer
  u <- optionMaybe $ identifier lexer
  return $ case n of
    Left i -> Num (fromInteger i) (toList u)
    Right f -> Num (fromFloatDigits f) (toList u)

singleUnit = identifier lexer <&> \u -> Num 1 [u]

table =
  [ [prefix "-" negate, prefix "+" id],
    [binaryComp "*" (*) AssocLeft, binaryComp "/" (/) AssocLeft],
    [binaryConv "+" (+) AssocLeft, binaryConv "-" (-) AssocLeft]
  ]

binaryConv name f = Infix (do reservedOp lexer name; return $ BinaryConv f)

binaryComp name f = Infix (do reservedOp lexer name; return $ BinaryComp f)

prefix name f = Prefix (do reservedOp lexer name; return $ Unary f)
