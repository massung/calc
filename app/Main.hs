{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import Calc.Eval
-- import Calc.Parser.Expr
-- import Data.Either.Extra
-- import Data.List as L
-- import Data.Map as M
-- import System.Console.CmdArgs

-- -- command line options
-- data Opts = Opts
--   { outputUnits :: Maybe String,
--     exprString :: String,
--     vars :: [String]
--   }
--   deriving (Data, Typeable, Show, Eq)

-- opts =
--   Opts
--     { exprString = def &= name "e" &= typ "EXPR" &= help "Expression",
--       outputUnits = def &= name "o" &= typ "UNITS" &= help "Output units",
--       vars = def &= args &= typ "VARS"
--     }
--     &= summary "calc v1.0, (c) Jeffrey Massung"

-- evalVars args = sequence [evalVar x v | (x, v) <- L.zip ["x", "y", "z"] (vars args)]
--   where
--     evalVar x v = mapRight (x,) $ parseExpr M.empty v

-- evalExpr args = do
--   varMap <- mapLeft show $ evalVars args
--   expr <- mapLeft show $ parseExpr (M.fromList varMap) (exprString args)
--   eval expr

-- run args = case evalExpr args of
--   Right result -> print result
--   Left err -> print err

-- main = cmdArgs opts >>= run

main = print "test"
