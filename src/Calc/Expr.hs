{-# LANGUAGE OverloadedStrings #-}

module Calc.Expr where

import Calc.Defs
import Calc.Error
import Calc.Lexer
import Calc.Parser
import Calc.Scalar
import Calc.Units
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

data Expr
  = Answer
  | Term Scalar
  | Convert Units Expr
  | Apply Def [Expr]
  | Unary (Scalar -> Scalar) Expr
  | Binary (Scalar -> Scalar -> Either Error Scalar) Expr Expr
  | BinaryConv (Scalar -> Scalar -> Either Error Scalar) Expr Expr

hasPlaceholder Answer = True
hasPlaceholder (Term _) = False
hasPlaceholder (Convert _ x) = hasPlaceholder x
hasPlaceholder (Unary _ x) = hasPlaceholder x
hasPlaceholder (Binary _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (BinaryConv _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (Apply _ xs) = any hasPlaceholder xs

exprParser :: Parsec String (Map String Def) Expr
exprParser = buildExpressionParser exprTable exprTerm

exprTerm =
  exprParens
    <|> brackets lexer exprApply
    <|> Term <$> scalarParser
    <|> Term . fromUnits <$> unitsTerm
    <|> exprAnswer
    <|> (do reserved lexer "true"; return $ Term $ fromBool True)
    <|> (do reserved lexer "false"; return $ Term $ fromBool False)

exprParens = do
  expr <- parens lexer exprParser
  u <- optionMaybe unitsTerm
  return $ case u of
    Nothing -> expr
    Just units -> Convert units expr

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
  defs <- getState
  s <- identifier lexer
  xs <- sepBy exprParser (lexeme lexer $ char ';')
  case M.lookup s defs of
    Just def -> return $ Apply def xs
    Nothing -> fail $ s ++ " ?"

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    [binary "^" powScalar AssocLeft],
    [binary "*" (binOp (*)) AssocLeft, binary "/" (binOp (/)) AssocLeft],
    [binaryConv "+" (binOp (+)) AssocLeft, binaryConv "-" (binOp (-)) AssocLeft],
    [binaryConv "==" (cmpOp (==)) AssocLeft, binaryConv "/=" (cmpOp (/=)) AssocLeft, binaryConv "<" (cmpOp (<)) AssocLeft, binaryConv ">" (cmpOp (>)) AssocLeft, binaryConv "<=" (cmpOp (<=)) AssocLeft, binaryConv ">=" (cmpOp (>=)) AssocLeft],
    [Postfix (do Convert <$> exprCast)]
  ]
  where
    binOp f x y = Right $ f x y
    cmpOp f x y = Right $ fromBool (f x y)

prefix op f = Prefix (do reservedOp lexer op; return $ Unary f)

binary op f = Infix (do reservedOp lexer op; return $ Binary f)

binaryConv op f = Infix (do reservedOp lexer op; return $ BinaryConv f)
