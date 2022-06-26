{-# LANGUAGE OverloadedStrings #-}

module Calc.Lexer where

import Text.Parsec
import Text.Parsec.Token

lexer = makeTokenParser lang
  where
    lang =
      LanguageDef
        { commentStart = "",
          commentEnd = "",
          commentLine = "#",
          nestedComments = False,
          identStart = letter,
          identLetter = letter,
          opStart = oneOf "_:+-*/",
          opLetter = parserZero,
          reservedNames = ["ans", "to"],
          reservedOpNames = ["_", "+", "-", "*", "/", ":"],
          caseSensitive = True
        }
