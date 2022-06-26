{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calc.Alias where

import Calc.Scalar
import Calc.Units as U hiding (units)
import Data.ByteString.UTF8 as BS
import Data.Csv
import Data.FileEmbed
import Data.Map as M
import Data.Vector as V hiding (mapMaybe, (++))
import Text.Parsec

data Alias = Alias
  { alias :: Unit,
    units :: Units,
    scale :: Double
  }
  deriving (Show)

instance FromNamedRecord Alias where
  parseNamedRecord r = do
    alias <- r .: "alias"
    units <- r .: "units"
    scale <- r .: "scale"
    return Alias {alias = alias, units = units, scale = scale}

instance FromField Units where
  parseField s = case parse unitsParser "aliases" (toString s) of
    Left err -> return $ U.fromList [] -- TODO: error
    Right units -> return units

instance FromField Unit where
  parseField = return . Unit . toString

aliases = do
  case decodeByName $(embedStringFile "res/aliases.csv") of
    Left err -> M.empty
    Right (_, rows) -> M.fromList $ V.toList $ V.map kv rows
  where
    kv a = (alias a, Scalar (scale a) (Just $ units a))
