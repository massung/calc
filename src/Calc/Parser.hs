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
  = Term Scalar
  | Unary String UnaryOp Expr
  | Binary String BinaryOp Expr Expr

instance Show Expr where
  show (Term x) = show x
  show (Unary n _ x) = n ++ show x
  show (Binary n _ x y) = show x ++ n ++ show y

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
      opStart = oneOf "_:+-*/",
      opLetter = oneOf "_:+-*/",
      reservedNames = ["ans", "to"],
      reservedOpNames = ["_", "+", "-", "*", "/", ":"],
      caseSensitive = True
    }

lexer = makeTokenParser calc

parseExpr = parse (ws >> expr) ""
  where
    ws = whiteSpace lexer

exprParser :: Parsec String () Expr
exprParser = buildExpressionParser exprTable exprTerm

expr = exprParser

exprTerm =
  parens lexer expr
    <|> brackets lexer expr
    <|> scalar
    <|> unitScalar

scalar = do
  n <- naturalOrFloat lexer
  return $ case n of
    Left i -> Term (Scalar (fromIntegral i) Nothing)
    Right f -> Term (Scalar f Nothing)

unitScalar = units <&> Term

units = many1 unit <&> Scalar 1 . Just . U.fromList
  where
    unit = do
      u <- identifier lexer <&> Unit
      n <- option 1 $ lexeme lexer (char '^') >> integer lexer
      return (u, n)

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    [postfixUnits],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
    --[unitsConversion]
  ]

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

postfixUnits = Postfix $ do
  optional $ reservedOp lexer "_"
  u <- units
  return $ Unary "_" (* u)

--unitsConversion = Postfix $ do
--  reservedOp lexer ":"
--  u <- units
--  return $ UnaryOp (" : " ++ show u) (conv u)
