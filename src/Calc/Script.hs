{-# LANGUAGE TemplateHaskell #-}

module Calc.Script where

import Calc.Defs
import Calc.Dims
import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Units
import Calc.Units
import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.FileEmbed
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Token

data Script = Script
  { units :: Map String Units,
    defs :: Map String Def
  }

builtInDefs :: IO (Map String Def)
builtInDefs = either (throw . ExprError) return $ loadScriptContents defMap "built-ins.tn" source
  where
    source = $(embedStringFile "scripts/functions.tn")

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
  loadScriptContents defs path contents

loadScriptContents defs path contents =
  case runParser scriptParser defs path contents of
    Right functions -> return $ union (M.fromList functions) defs
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

scriptArgs = brackets lexer (sepBy (typedArg <|> anyArg <|> noneArg) $ lexeme lexer (char ';'))
  where
    anyArg = do reserved lexer "any"; return Any
    noneArg = do reserved lexer "none"; return None
    typedArg = do
      (dim, e) <- dimParser
      return $ Typed $ mapDims (* e) (baseDims dim)
