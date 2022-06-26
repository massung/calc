{-# LANGUAGE OverloadedStrings #-}

module Calc.Units where

import Calc.Lexer
import Data.ByteString.UTF8 as BS
import Data.Csv
import Data.Foldable as F
import Data.Functor
import Data.List as L
import Data.Map as M
import Data.String
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

newtype Unit = Unit String
  deriving (Eq, Ord)

instance Show Unit where
  show (Unit u) = u

instance IsString Unit where
  fromString = Unit

instance FromField Unit where
  parseField s = pure $ Unit (BS.toString s)

newtype Units = Units (Map Unit Double)
  deriving (Eq)

instance Show Units where
  show (Units u) =
    case (F.null num, F.null den) of
      (False, False) -> expNum num ++ "/" ++ expNum den
      (False, _) -> expNum num
      (_, False) -> expDen den
      _ -> ""
    where
      (num, den) = L.partition ((> 0) . snd) $ M.toList u

      --
      showUnits (u, 1) = show u
      showUnits (u, n) = show u ++ "^" ++ show n

      --
      showUnitsAbs (u, 1) = show u
      showUnitsAbs (u, -1) = show u
      showUnitsAbs (u, n) = show u ++ "^" ++ show (abs n)

      -- show the numerator
      expNum = unwords . L.map showUnitsAbs
      expDen = unwords . L.map showUnits

fromList = Units . M.fromListWith (+)

singleton u = Units $ M.singleton u 1

recipUnits (Units a) =
  Units $ M.map negate a

multiplyUnits (Units a) (Units b) =
  Units $ M.filter (/= 0) $ M.unionWith (+) a b

divideUnits a b =
  multiplyUnits a $ recipUnits b

expUnits (Units u) n =
  Units $ M.map (* n) u

unitsParser :: Parsec String () Units
unitsParser = buildExpressionParser unitsTable unitsTerm

unitsTerm = parens lexer units <|> units

units = many1 unit <&> Calc.Units.fromList
  where
    unit = do
      u <- identifier lexer <&> Unit
      n <- option 1 unitsExponent
      return (u, n)

unitsExponent = do
  reservedOp lexer "^"
  sign <- option 1 (reservedOp lexer "-" >> return (-1))
  n <- naturalOrFloat lexer
  return $ case n of
    Left i -> sign * fromInteger i
    Right f -> sign * f

unitsTable =
  [ [ Infix (do reservedOp lexer "*"; return multiplyUnits) AssocLeft,
      Infix (do reservedOp lexer "/"; return divideUnits) AssocLeft
    ]
  ]
