{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr where

import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units
import Data.Map as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

data Expr
  = Term Scalar
  | Convert Expr Units
  | Unary String (Scalar -> Scalar) Expr
  | Binary String (Scalar -> Scalar -> Scalar) Expr Expr

instance Show Expr where
  show (Term x) = show x
  show (Convert x u) = show x ++ " : " ++ show u
  show (Unary op _ x) = "(" ++ op ++ show x ++ ")"
  show (Binary op _ x y) = "(" ++ show x ++ op ++ show y ++ ")"

exprParser :: Parsec String Scalar Expr
exprParser = buildExpressionParser exprTable exprTerm

exprTerm = do
  e <- expr
  u <- optionMaybe exprCast
  return $ case u of
    Nothing -> e
    Just u -> Convert e u

exprCast = (reservedOp lexer ":" <|> reserved lexer "to") >> unitsParser

expr =
  parens lexer exprTerm
    <|> Term <$> scalarParser
    <|> Term . fromUnits <$> unitsTerm
    <|> Term <$> exprAnswer

exprAnswer = do
  reserved lexer "_"
  getState

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    --[binary "^" (^) AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
  ]

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)
