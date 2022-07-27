module Calc.Parser.Script where

import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Units
import Text.Parsec
import Text.Parsec.Token

loadScript path = do
  contents <- readFile path
  return $ parse scriptParser path contents

scriptParser = do
  whiteSpace lexer
  functions <- many scriptFunction
  eof
  return functions

scriptFunction = do
  reserved lexer "function"
  def <- identifier lexer
  units <- scriptUnits
  reservedOp lexer "="
  expr <- exprParser
  return (def, units, expr)

scriptUnits = brackets lexer (sepBy unitsParser $ char ';')
