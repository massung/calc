{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Units where

import Calc.Units.Base
import Calc.Units.Lexer
import Calc.Units.SI
import Data.ByteString.UTF8 as BS
import Data.Csv
import Data.FileEmbed
import Data.Foldable as F
import Data.List as L
import Data.Map as M hiding ((++), (\\))
import Data.Maybe
import Data.String as S
import Data.Tuple.Extra
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token hiding (symbol)

newtype Units = Units (Map Unit Double)
  deriving (Eq, Ord)

instance IsString Units where
  fromString s = case parse unitsParser "" s of
    Left err -> error $ show err
    Right units -> units

instance FromField Units where
  parseField = pure . S.fromString . BS.toString

instance Show Units where
  show (Units u)
    | F.null den = showUnits num
    | F.null num = showUnits den
    | otherwise = showUnits num ++ "/" ++ showUnits (L.map (second abs) den)
    where
      (num, den) = L.partition ((> 0) . snd) $ M.toList u

      -- show unit symbol and optional exponent
      showUnit (u, 1) = show u
      showUnit (u, n) = show u ++ "^" ++ show n

      -- show list of unit symbols
      showUnits = unwords . L.map showUnit

instance IsString Unit where
  fromString s = fromMaybe (error $ "unknown unit: " ++ s) $ M.lookup s unitsMap

instance FromField Unit where
  parseField = pure . S.fromString . BS.toString

units = case decodeUnits $(embedStringFile "res/units.csv") of
  Left err -> error $ show err
  Right units -> units

siUnits = case decodeSIUnits $(embedStringFile "res/siUnits.csv") of
  Left err -> error $ show err
  Right units -> units

unitsMap = M.fromList [(symbol u, u) | u <- units ++ allSIUnits]
  where
    allSIUnits = L.concat [u : L.map fst si | (u, si) <- siUnits]

unitsDim (Units u) = M.fromListWith (+) [(dim u, e) | (u, e) <- M.toList u]

singletonUnits u = Units $ M.singleton u 1

recipUnits (Units a) = Units $ M.map negate a

multiplyUnits (Units a) (Units b) = Units $ M.filter (/= 0) $ M.unionWith (+) a b

divideUnits a b = multiplyUnits a $ recipUnits b

expUnits (Units u) n = Units $ M.map (* n) u

simplifyUnits (Units from) (Units to) = simplify (sort $ M.toList from) (sort $ M.toList to)
  where
    simplify [] _ = Nothing
    simplify _ [] = Nothing
    simplify ((u1, e1) : xs) ((u2, e2) : ys)
      | u1 /= u2 = Nothing
      | otherwise = simplifyExp xs ys (e1 / e2)

    simplifyExp [] [] factor = Just factor
    simplifyExp [] _ factor = Nothing
    simplifyExp _ [] factor = Nothing
    simplifyExp ((u1, e1) : xs) ((u2, e2) : ys) factor
      | u1 /= u2 = Nothing
      | e1 / e2 /= factor = Nothing
      | otherwise = simplifyExp xs ys factor

unitsParser :: Parsec String () Units
unitsParser = buildExpressionParser unitsExprTable unitsTerm

unitsTerm = parens unitsLexer terms <|> terms
  where
    terms = Units . M.fromList <$> many1 unitTerm

unitTerm = do
  u <- S.fromString <$> identifier unitsLexer
  n <- option 1 $ lexeme unitsLexer unitExponent
  return (u, n)

unitExponent = do
  reservedOp unitsLexer "^"
  s <- exponentSign
  e <- (fromInteger <$> decimal unitsLexer) <|> float unitsLexer
  return (e * s)

exponentSign = option 1 (neg <|> pos)
  where
    neg = reservedOp unitsLexer "-" >> return -1
    pos = reservedOp unitsLexer "+" >> return 1

unitsExprTable =
  [ [ Infix (do reservedOp unitsLexer "*"; return multiplyUnits) AssocLeft,
      Infix (do reservedOp unitsLexer "/"; return divideUnits) AssocLeft
    ]
  ]
