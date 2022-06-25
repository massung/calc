{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser where

import Calc.Scalar
import Calc.Units as U
import Data.Functor
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

-- valid expressions
data Expr
  = Term Scalar
  | Convert Expr Units
  | Unary String UnaryOp Expr
  | Binary String BinaryOp Expr Expr
  | BinaryConvert String BinaryOp Expr Expr

instance Show Expr where
  show (Term x) = show x
  show (Convert x u) = show x ++ " : " ++ show u
  show (Unary n _ x) = n ++ show x
  show (Binary n _ x y) = show x ++ n ++ show y
  show (BinaryConvert n _ x y) = show x ++ n ++ show y

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
      opLetter = parserZero,
      reservedNames = ["ans", "to"],
      reservedOpNames = ["_", "+", "-", "*", "/", ":"],
      caseSensitive = True
    }

lexer = makeTokenParser calc

parseExpr = parse (whiteSpace lexer >> parser) ""
  where
    parser = do
      e <- expr
      eof
      return e

exprParser :: Parsec String () Expr
exprParser = buildExpressionParser exprTable exprTerm

unitsParser :: Parsec String () Units
unitsParser = buildExpressionParser unitsTable unitsTerm

expr = do
  e <- exprParser
  u <- optionMaybe $ reservedOp lexer ":" >> unitsParser
  return $ case u of
    Nothing -> e
    Just u -> Convert e u

exprTerm = parens lexer expr <|> scalar

scalar = do
  n <- naturalOrFloat lexer
  u <- scalarUnits
  return $ case n of
    Left i -> Term (Scalar (fromIntegral i) u)
    Right f -> Term (Scalar f u)

scalarUnits = optionMaybe $ do
  optional $ reservedOp lexer "_"
  try unitsParser <|> units

unitsTerm = parens lexer units <|> units

units = many1 unit <&> U.fromList
  where
    unit = do
      u <- identifier lexer <&> Unit
      n <- option 1 unitsExponent
      return (u, n)

unitsExponent = do
  reservedOp lexer "^"
  sign <- option 1 (reservedOp lexer "-" >> return (-1))
  n <- naturalOrFloat lexer
  return $ case n of
    Left i -> sign * fromInteger i
    Right f -> sign * f

unitsTable =
  [ [ Infix (do reservedOp lexer "*"; return multiplyUnits) AssocLeft,
      Infix (do reservedOp lexer "/"; return divideUnits) AssocLeft
    ]
  ]

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    [binary "^" expScalar AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binaryConvert "+" (+) AssocLeft, binaryConvert "-" (-) AssocLeft]
  ]

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)

binaryConvert name f = Infix (do reservedOp lexer name; return $ BinaryConvert name f)
