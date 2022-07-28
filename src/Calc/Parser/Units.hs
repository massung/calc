{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Units where

import Calc.Parser.Lexer
import Calc.Units
import Data.Either
import Data.Map.Strict as M
import Data.String
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

instance IsString Units where
  fromString = fromRight (error "illegal units") . parseUnits

parseUnits = parse parser ""
  where
    parser = do
      whiteSpace lexer
      u <- unitsParser
      eof
      return u

unitsParser :: Parsec String st Units
unitsParser = buildExpressionParser unitsExprTable unitsTerm

unitsTerm = parens lexer unitsParser <|> terms
  where
    terms = do
      units <- mconcat <$> many1 unitTerm
      if validateUnits units
        then return units
        else fail "illegal units"

unitTerm = do
  s <- identifier lexer
  case unitMap !? s of
    Nothing -> fail $ "unknown unit: " ++ s
    Just u -> do
      n <- option 1 $ lexeme lexer unitExponent
      return $ Units (M.singleton u n)

unitExponent = do
  reservedOp lexer "^"
  s <- unitsSign
  e <- either fromInteger toRational <$> naturalOrFloat lexer
  return (e * s)

unitsSign = option 1 (neg <|> pos)
  where
    neg = reservedOp lexer "-" >> return -1
    pos = reservedOp lexer "+" >> return 1

unitsExprTable =
  [ [ Infix (do reservedOp lexer "*"; return (<>)) AssocLeft,
      Infix (do reservedOp lexer "/"; return (</>)) AssocLeft
    ]
  ]
