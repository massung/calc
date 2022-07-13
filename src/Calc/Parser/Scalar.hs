{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Scalar where

import Calc.Parser.Lexer
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units
import Data.Either
import Data.String
import Text.Parsec
import Text.Parsec.Token

instance IsString Scalar where
  fromString s = fromRight (error "no parse") $ parse parser "" s
    where
      parser = do
        x <- scalarParser
        eof
        return x

scalarParser = do
  n <- naturalOrFloat lexer
  u <- option noUnits $ try unitsParser <|> unitsTerm
  return $ case n of
    Left i -> Scalar (fromIntegral i) u
    Right f -> Scalar (toRational f) u
