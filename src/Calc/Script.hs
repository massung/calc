module Calc.Script where

import Calc.Defs
import Calc.Dims
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

scriptDef :: [Arg] -> Expr -> Def
scriptDef args expr = Def f args
  where
    f xs = case mapArgs xs args of
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
  args <- scriptArgs
  reservedOp lexer "="
  expr <- exprParser
  return (def, scriptDef args expr)

scriptArgs = brackets lexer (sepBy (typedArg <|> anyArg) $ lexeme lexer (char ';'))
  where
    anyArg = do reserved lexer "_"; return Any
    typedArg = do
      (dim, e) <- dimParser
      return $ Typed $ mapDims (* e) (baseDims dim)
