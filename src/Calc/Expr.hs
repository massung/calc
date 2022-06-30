{-# LANGUAGE OverloadedStrings #-}

module Calc.Expr where

import Calc.Lexer
import Calc.Scalar
import Calc.Units
import Data.Map as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

-- valid expressions
data Expr
  = Term Scalar
  | Convert Expr Units
  | Unary String UnaryOp Expr
  | Binary String BinaryOp Expr Expr

instance Show Expr where
  show (Term x) = show x
  show (Convert x u) = show x ++ " : " ++ show u
  show (Unary n _ x) = n ++ show x
  show (Binary n _ x y) = show x ++ n ++ show y

-- unary expression operators
type UnaryOp = Scalar -> Scalar

-- binary expression operators
type BinaryOp = Scalar -> Scalar -> Scalar

parseExpr vars = runParser (whiteSpace lexer >> parser) vars ""
  where
    parser = do
      e <- expr
      eof
      return e

exprParser :: Parsec String (Map String Expr) Expr
exprParser = buildExpressionParser exprTable exprTerm

expr = do
  e <- exprParser
  u <- optionMaybe $ reservedOp lexer ":" >> unitsParser
  return $ case u of
    Nothing -> e
    Just u -> Convert e u

exprTerm =
  parens lexer expr
    <|> Term <$> scalarParser
    <|> Term . Scalar 1 . Just <$> unitsTerm
    <|> lexeme lexer exprVariable

exprVariable = do
  var <- char '?' >> identifier lexer
  vars <- getState
  case M.lookup var vars of
    Nothing -> fail $ "undefined variable: " ++ var
    Just expr -> pure expr

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    [binary "^" expScalar AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
  ]

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)
