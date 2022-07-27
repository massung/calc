{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr where

import Calc.Defs
import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units
import Data.String
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

data Expr
  = Answer
  | Term Scalar
  | Convert Units Expr
  | Apply Def [Expr]
  | Unary (Scalar -> Scalar) Expr
  | Binary (Scalar -> Scalar -> Scalar) Expr Expr
  | BinaryConv (Scalar -> Scalar -> Scalar) Expr Expr

hasPlaceholder Answer = True
hasPlaceholder (Term _) = False
hasPlaceholder (Convert _ x) = hasPlaceholder x
hasPlaceholder (Unary _ x) = hasPlaceholder x
hasPlaceholder (Binary _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (BinaryConv _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (Apply _ xs) = any hasPlaceholder xs

exprParser :: Parsec String () Expr
exprParser = buildExpressionParser exprTable exprTerm

exprTerm =
  parens lexer exprParser
    <|> brackets lexer exprApply
    <|> Term <$> scalarParser
    <|> Term . fromUnits <$> unitsTerm
    <|> exprAnswer

exprAnswer = do
  reserved lexer "_"
  u <- optionMaybe unitsTerm
  return $ case u of
    Nothing -> Answer
    Just units -> Convert units Answer

exprCast = do
  reservedOp lexer ":" <|> reserved lexer "to"
  unitsParser

exprApply = do
  def <- identifier lexer
  xs <- sepBy exprParser (char ';')
  return $ Apply (fromString def) xs

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    --[binary "^" (^) AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binaryConv "+" (+) AssocLeft, binaryConv "-" (-) AssocLeft],
    [Postfix (do Convert <$> exprCast)]
  ]

prefix op f = Prefix (do reservedOp lexer op; return $ Unary f)

binary op f = Infix (do reservedOp lexer op; return $ Binary f)

binaryConv op f = Infix (do reservedOp lexer op; return $ BinaryConv f)
