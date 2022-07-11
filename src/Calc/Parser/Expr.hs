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

exprParser :: Parsec String (Map String Expr) Expr
exprParser = buildExpressionParser exprTable expr

expr = do
  e <- exprTerm
  u <- optionMaybe $ reservedOp lexer ":" >> unitsParser
  return $ case u of
    Nothing -> e
    Just u -> Convert e u

exprTerm =
  parens lexer expr
    <|> Term <$> scalarParser
    <|> Term . fromUnits <$> unitsTerm
    <|> lexeme lexer exprVariable

exprVariable = do
  var <- char '?' >> identifier lexer
  vars <- getState
  case M.lookup var vars of
    Nothing -> fail $ "undefined variable: " ++ var
    Just expr -> pure expr

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    --[binary "^" expScalar AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
  ]

prefix name f = Prefix (do reservedOp lexer name; return $ Unary name f)

binary name f = Infix (do reservedOp lexer name; return $ Binary name f)
