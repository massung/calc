{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr where

import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

data Expr
  = Answer
  | Term Scalar
  | Convert Expr Units
  | Unary String (Scalar -> Scalar) Expr
  | Binary String (Scalar -> Scalar -> Scalar) Expr Expr
  | BinaryConv String (Scalar -> Scalar -> Scalar) Expr Expr

instance Show Expr where
  show Answer = "_"
  show (Term x) = show x
  show (Convert x u) = show x ++ " : " ++ show u
  show (Unary op _ x) = "(" ++ op ++ show x ++ ")"
  show (Binary op _ x y) = "(" ++ show x ++ op ++ show y ++ ")"
  show (BinaryConv op _ x y) = "(" ++ show x ++ op ++ show y ++ ")"

exprParser :: Parsec String () Expr
exprParser = buildExpressionParser exprTable exprTerm

exprTerm = do
  e <- expr
  u <- optionMaybe exprCast
  return $ case u of
    Nothing -> e
    Just u -> Convert e u

exprCast = do
  reservedOp lexer ":" <|> reserved lexer "to"
  unitsParser

expr =
  parens lexer exprParser
    <|> Term <$> scalarParser
    <|> Term . fromUnits <$> unitsTerm
    <|> exprAnswer

exprAnswer = do
  reserved lexer "_"
  u <- optionMaybe unitsTerm
  return $ case u of
    Nothing -> Answer
    Just units -> Convert Answer units

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    --[binary "^" (^) AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binaryConv "+" (+) AssocLeft, binaryConv "-" (-) AssocLeft]
  ]

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)

binaryConv name f = Infix (do reservedOp lexer name; return $ BinaryConv name f)
