{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Scalar where

import Calc.Parser.Lexer
import Calc.Parser.Units
import Calc.Scalar
import Data.Either
import Data.String
import Text.Parsec
import Text.Parsec.Token

instance IsString Scalar where
  fromString = fromRight (error "no parse") . parseScalar

parseScalar = parse parser ""
  where
    parser = do
      s <- unitsSign
      n <- scalarParser
      eof
      if s < 0
        then return $ negate n
        else return n

scalarParser = do
  n <- naturalOrFloat lexer
  u <- option mempty $ try unitsParser <|> unitsTerm
  return $ case n of
    Left i -> Scalar (fromIntegral i) u
    Right f -> Scalar (toRational f) u
