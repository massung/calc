{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Dims where

import Calc.Lexer
import Data.Char
import Data.List.Extra as L
import Data.Map.Strict as M
import Data.Ratio
import Text.Parsec
import Text.Parsec.Token

data Dim
  = Angle
  | Area
  | Capacitance
  | Charge
  | Current
  | Duration
  | Energy
  | Force
  | Frequency
  | Length
  | Mass
  | Power
  | Pressure
  | Resistance
  | Speed
  | Storage
  | Voltage
  | Volume
  deriving (Eq, Ord, Show)

newtype Dims = Dims (Map Dim Rational)
  deriving (Eq, Ord)

instance Show Dims where
  show (Dims dims) = "[" ++ mconcat (L.intersperse ";" [showDim dim | dim <- M.toList dims]) ++ "]"
    where
      showDim (dim, 1) = L.lower $ show dim
      showDim (dim, n)
        | denominator n == 1 = L.lower (show dim) ++ "^" ++ show (numerator n)
        | otherwise = L.lower (show dim) ++ "^" ++ show (fromRational n)

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/= 0) $ M.unionWith (+) a b

instance Monoid Dims where
  mempty = Dims M.empty

baseDims :: Dim -> Dims
baseDims Angle = Dims [(Angle, 1)]
baseDims Area = Dims [(Length, 2)]
baseDims Capacitance = Dims [(Duration, 4), (Current, 2), (Mass, -1), (Length, -2)]
baseDims Charge = Dims [(Current, 1), (Duration, 1)]
baseDims Current = Dims [(Current, 1)]
baseDims Duration = Dims [(Duration, 1)]
baseDims Energy = Dims [(Mass, 1), (Length, 2), (Duration, -2)]
baseDims Force = Dims [(Mass, 1), (Length, 1), (Duration, -2)]
baseDims Frequency = Dims [(Duration, -1)]
baseDims Length = Dims [(Length, 1)]
baseDims Mass = Dims [(Mass, 1)]
baseDims Power = Dims [(Mass, 1), (Length, 2), (Duration, -3)]
baseDims Pressure = Dims [(Mass, 1), (Length, -1), (Duration, -2)]
baseDims Resistance = Dims [(Mass, 1), (Length, 2), (Duration, -3), (Current, -4)]
baseDims Speed = Dims [(Length, 1), (Duration, -1)]
baseDims Storage = Dims [(Storage, 1)]
baseDims Voltage = Dims [(Mass, 1), (Length, 2), (Duration, -3), (Current, -1)]
baseDims Volume = Dims [(Length, 3)]

nullDims (Dims dims) = M.null dims

mapDims f (Dims dims) = Dims $ M.map f dims

recipDims = mapDims negate

powDims n = mapDims (* n)

-- parsing -------------------------------------------------

dimParser :: Parsec String st (Dim, Rational)
dimParser = do
  s <- identifier lexer >>= fromString
  e <- option 1 parseExponent
  return (s, e)
  where
    fromString s
      | s == "angle" = return Angle
      | s == "area" = return Area
      | s == "capacitance" = return Capacitance
      | s == "charge" = return Charge
      | s == "current" = return Current
      | s == "duration" = return Duration
      | s == "energy" = return Energy
      | s == "force" = return Force
      | s == "frequency" = return Frequency
      | s == "length" = return Length
      | s == "mass" = return Mass
      | s == "power" = return Power
      | s == "pressure" = return Pressure
      | s == "resistance" = return Resistance
      | s == "speed" = return Speed
      | s == "storage" = return Storage
      | s == "voltage" = return Voltage
      | s == "volume" = return Volume
      | otherwise = fail $ "no dimension " ++ s

dimsParser :: Parsec String st Dims
dimsParser = do
  dims <- brackets lexer $ sepBy dimParser (lexeme lexer $ char ';')
  return $ L.foldl' (<>) mempty [powDims e $ baseDims dim | (dim, e) <- dims]

parseExponent = do
  reservedOp lexer "^"
  s <- option 1 (neg <|> pos)
  e <- either fromInteger toRational <$> naturalOrFloat lexer
  return (e * s)
  where
    neg = reservedOp lexer "-" >> return -1
    pos = reservedOp lexer "+" >> return 1
