{-# LANGUAGE OverloadedStrings #-}

module Calc.Units.Base where

import Data.ByteString.Lazy as BS
import Data.Csv
import Data.Vector as V

data Unit = Unit {dim :: String, name :: String, symbol :: String}
  deriving (Eq, Ord)

instance Show Unit where
  show = symbol

instance FromNamedRecord Unit where
  parseNamedRecord r = do
    dim <- r .: "dim"
    name <- r .: "name"
    symbol <- r .: "symbol"
    return $ Unit {dim = dim, name = name, symbol = symbol}

decodeUnits :: BS.ByteString -> Either String [Unit]
decodeUnits s = V.toList . snd <$> decodeByName s
