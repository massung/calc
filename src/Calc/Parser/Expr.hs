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
  | Convert Units Expr
  | Unary String (Scalar -> Scalar) Expr
  | Binary String (Scalar -> Scalar -> Scalar) Expr Expr
  | BinaryConv String (Scalar -> Scalar -> Scalar) Expr Expr

hasPlaceholder Answer = True
hasPlaceholder (Term _) = False
hasPlaceholder (Convert _ x) = hasPlaceholder x
hasPlaceholder (Unary _ _ x) = hasPlaceholder x
hasPlaceholder (Binary _ _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (BinaryConv _ _ x y) = hasPlaceholder x || hasPlaceholder y

exprParser :: Parsec String () Expr
exprParser = buildExpressionParser exprTable exprTerm

exprTerm =
  parens lexer exprParser
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

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    --[binary "^" (^) AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binaryConv "+" (+) AssocLeft, binaryConv "-" (-) AssocLeft],
    [postfix]
  ]

postfix = Postfix (do Convert <$> exprCast)

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)

binaryConv name f = Infix (do reservedOp lexer name; return $ BinaryConv name f)
