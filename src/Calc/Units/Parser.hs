{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Units.Parser where

import Calc.Units.Compound
import Data.Functor.Identity
import Data.String
import Data.Map as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

unitsLexer :: GenTokenParser String () Identity
unitsLexer = makeTokenParser lang
  where
    lang =
      LanguageDef
        { commentStart = "",
          commentEnd = "",
          commentLine = "",
          nestedComments = False,
          identStart = letter,
          identLetter = letter,
          opStart = oneOf "+-*/^",
          opLetter = parserZero,
          reservedNames = ["ans", "to"],
          reservedOpNames = ["+", "-", "*", "/", "^"],
          caseSensitive = True
        }

unitsParser :: Parsec String () Units
unitsParser = buildExpressionParser exprTable unitsTerm

unitsTerm = parens unitsLexer units <|> units

units = Units . M.fromList <$> many1 unit
  where
    unit = do
      u <- fromString <$> identifier unitsLexer
      n <- option 1 unitsExponent
      return (u, n)

unitsExponent = do
  reservedOp unitsLexer "^"
  s <- exponentSign
  e <- (fromInteger <$> decimal unitsLexer) <|> float unitsLexer
  return (e * s)

exponentSign = option 1 (neg <|> pos)
  where
    neg = reservedOp unitsLexer "-" >> return -1
    pos = reservedOp unitsLexer "+" >> return 1

exprTable =
  [ [ Infix (do reservedOp unitsLexer "*"; return multiplyUnits) AssocLeft,
      Infix (do reservedOp unitsLexer "/"; return divideUnits) AssocLeft
    ]
  ]
