{-# LANGUAGE OverloadedStrings #-}

module Calc.Conv where

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
  [ ("atto", "a", Linear 1e18),
    ("femto", "f", Linear 1e15),
    ("pico", "p", Linear 1e12),
    ("nano", "n", Linear 1e9),
    ("micro", "u", Linear 1e6),
    ("milli", "m", Linear 1e3),
    ("centi", "c", Linear 1e2),
    ("deci", "d", Linear 1e1),
    ("deca", "da", Linear 1e-1),
    ("hecto", "h", Linear 1e-2),
    ("kilo", "k", Linear 1e-3),
    ("mega", "M", Linear 1e-6),
    ("giga", "G", Linear 1e-9),
    ("tera", "T", Linear 1e-12),
    ("peta", "P", Linear 1e-15),
    ("exa", "E", Linear 1e-18)
  ]

recipConv Base = Base
recipConv (Linear x) = Linear $ recip x
recipConv (Function f) = Function $ recip . f

powConv _ Base = Base
powConv n (Linear x) = Linear $ x ^^ n
powConv n (Function f) = Function f

applyConv Base w = w
applyConv (Linear x) w = w * x
applyConv (Function f) w = f w
