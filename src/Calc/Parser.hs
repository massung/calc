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
      opStart = oneOf "+-*/",
      opLetter = oneOf "+-*/",
      reservedNames = ["ans", "to"],
      reservedOpNames = ["+", "-", "*", "/"],
      caseSensitive = True
    }

lexer = makeTokenParser calc

parseExpr = parse (ws >> (expr <|> unitsExpr)) ""
  where
    ws = whiteSpace lexer

exprParser :: Parsec String () Expr
exprParser = buildExpressionParser exprTable exprTerm

unitsExprParser :: Parsec String () Expr
unitsExprParser = buildExpressionParser unitsExprTable unitsTerm

expr = do
  e <- exprParser
  u <- optionMaybe (try unitsExpr <|> units)
  return $ case u of
    Nothing -> e
    Just u' -> Binary "_" (*) e u'

exprTerm =
  parens lexer expr
    <|> brackets lexer expr
    <|> scalar

unitsExpr = unitsExprParser

unitsTerm =
  parens lexer unitsExpr
    <|> brackets lexer unitsExpr
    <|> units

scalar = do
  n <- naturalOrFloat lexer
  return $ case n of
    Left i -> Term (Scalar (fromIntegral i) Nothing)
    Right f -> Term (Scalar f Nothing)

units = many1 unit <&> Term . Scalar 1 . Just . U.fromList
  where
    unit = do
      u <- identifier lexer <&> Unit
      n <- option 1 $ lexeme lexer (char '^') >> integer lexer
      return (u, n)

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
  ]

unitsExprTable =
  [ [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft]
  ]

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

postfixUnits = Postfix $ do
  (Term u) <- unitsTerm
  return $ Unary "_" (* u)
