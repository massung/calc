{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where

import Data.Ratio

data Conv
  = Base
  | Linear Rational
  | Function (Rational -> Rational)

instance Semigroup Conv where
  (<>) Base y = y
  (<>) x Base = x
  (<>) (Linear x) (Linear y) = Linear $ x * y
  (<>) (Linear x) (Function f) = Function $ (* x) . f
  (<>) (Function f) (Linear y) = Function $ f . (* y)
  (<>) (Function f) (Function g) = Function $ f . g

instance Show Conv where
  show Base = "Base"
  show (Linear x) = "Linear " ++ show x
  show (Function f) = "Function"

siConversions =
  [ ("a", Linear 1e18),
    ("f", Linear 1e15),
    ("p", Linear 1e12),
    ("n", Linear 1e9),
    ("u", Linear 1e6),
    ("m", Linear 1e3),
    ("c", Linear 1e2),
    ("d", Linear 1e1),
    ("da", Linear 1e-1),
    ("h", Linear 1e-2),
    ("k", Linear 1e-3),
    ("M", Linear 1e-6),
    ("G", Linear 1e-9),
    ("T", Linear 1e-12),
    ("P", Linear 1e-15),
    ("E", Linear 1e-18)
  ]

storageConversions =
  [ ("k", Linear (1 % 1024)),
    ("M", Linear (1 % 1048576)),
    ("G", Linear (1 % 1073741824)),
    ("T", Linear (1 % 1099511627776)),
    ("P", Linear (1 % 1125899906842624)),
    ("E", Linear (1 % 1152921504606846976))
  ]

recipConv Base = Base
recipConv (Linear x) = Linear $ recip x
recipConv (Function f) = Function $ recip . f

powConv _ Base = Base
powConv n (Linear x) =
  if denominator n == 1
    then Linear $ x ^^ numerator n
    else Linear $ toRational $ fromRational x ** fromRational n
powConv n (Function f) = Function f

applyConv Base w = w
applyConv (Linear x) w = w * x
applyConv (Function f) w = f w
