{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Units where

import Calc.Parser.Lexer
import Calc.Units
import Data.ByteString.UTF8 as BS hiding (fromString)
import Data.Csv
import Data.Either
import Data.String
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

instance IsString Units where
  fromString s = fromRight (error "no parse") $ parse parser "" s
    where
      parser = do
        x <- unitsParser
        eof
        return x

instance FromField Units where
  parseField = pure . fromString . BS.toString

unitsParser :: Parsec String st Units
unitsParser = buildExpressionParser unitsExprTable unitsTerm

unitsTerm = parens lexer terms <|> terms
  where
    terms = mconcat <$> many1 unitTerm

unitTerm = do
  u <- fromString <$> identifier lexer
  n <- try (lexeme lexer unitExponent) <|> return 1
  return $ fromUnit u n

unitExponent = do
  reservedOp lexer "^"
  s <- unitsSign
  e <- decimal lexer
  return (e * s)

unitsSign = option 1 (neg <|> pos)
  where
    neg = reservedOp lexer "-" >> return -1
    pos = reservedOp lexer "+" >> return 1

unitsExprTable =
  [ [ Infix (do reservedOp lexer "*"; return (<>)) AssocLeft,
      Infix (do reservedOp lexer "/"; return divideUnits) AssocLeft
    ]
  ]
