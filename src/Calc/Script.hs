module Calc.Script where

import Calc.Defs
import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Units
import Calc.Scalar
import Calc.Units
import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Token

newtype Script = Script (Map String Def)

scriptDef :: [Units] -> Expr -> Def
scriptDef units expr = Def f units
  where
    f xs = case zipWithM convert xs units of
      Right xs -> evalState (runExceptT $ evalExpr expr) xs
      Left e -> Left e

loadScripts defs [] = return defs
loadScripts defs (path : rest) = loadScript defs path >>= (`loadScripts` rest)

loadScript defs path = do
  contents <- readFile path
  case runParser scriptParser defs path contents of
    Right functions -> return $ union (M.fromList functions) defMap
    Left err -> throw $ ExprError err

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
  return (def, scriptDef units expr)

scriptUnits = brackets lexer (sepBy unitsParser $ lexeme lexer (char ';'))
