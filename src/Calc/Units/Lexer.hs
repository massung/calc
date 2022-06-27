module Calc.Units.Lexer where

import Data.Functor.Identity
import Text.Parsec
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
