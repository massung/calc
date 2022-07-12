{-# LANGUAGE OverloadedStrings #-}

module Calc.SI where

siPrefixes =
  [ ("atto", "a", 1e-18),
    ("femto", "f", 1e-15),
    ("pico", "p", 1e-12),
    ("nano", "n", 1e-9),
    ("micro", "u", 1e-6),
    ("milli", "m", 1e-3),
    ("centi", "c", 1e-2),
    ("deci", "d", 1e-1),
    ("deca", "da", 1e1),
    ("hecto", "h", 1e2),
    ("kilo", "k", 1e3),
    ("mega", "M", 1e6),
    ("giga", "G", 1e9),
    ("tera", "T", 1e12),
    ("peta", "P", 1e15),
    ("exa", "E", 1e18)
  ]

siStoragePrefixes =
  [p | p@(_, _, x) <- siPrefixes, x > 1.0]
