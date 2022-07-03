{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Scalar where

import Calc.Parser.Lexer
import Calc.Parser.Units
import Calc.Scalar
import Data.ByteString.UTF8 as BS hiding (fromString)
import Data.Csv
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

instance FromField Scalar where
  parseField = pure . fromString . BS.toString

scalarParser = do
  n <- naturalOrFloat lexer
  u <- optionMaybe $ try unitsParser <|> unitsTerm
  return $ case n of
    Left i -> Scalar (fromIntegral i) u
    Right f -> Scalar f u
