{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Lexer where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Token

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser lang
  where
    lang =
      LanguageDef
        { commentStart = "",
          commentEnd = "",
          commentLine = "#",
          nestedComments = False,
          identStart = letter <|> char '_',
          identLetter = letter,
          opStart = oneOf ":+-*/<>=",
          opLetter = oneOf "=",
          reservedNames = ["_", "any", "none", "to", "function", "true", "false"],
          reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "=", ":"],
          caseSensitive = True
        }
