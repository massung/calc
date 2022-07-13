{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.SI where

import Data.Scientific

-- NOTE: Use of scientific so recip is exact.

siPrefixes =
  [ ("atto", "a", scientific 1 -18),
    ("femto", "f", scientific 1 -15),
    ("pico", "p", scientific 1 -12),
    ("nano", "n", scientific 1 -9),
    ("micro", "u", scientific 1 -6),
    ("milli", "m", scientific 1 -3),
    ("centi", "c", scientific 1 -2),
    ("deci", "d", scientific 1 -1),
    ("deca", "da", scientific 1 1),
    ("hecto", "h", scientific 1 2),
    ("kilo", "k", scientific 1 3),
    ("mega", "M", scientific 1 6),
    ("giga", "G", scientific 1 9),
    ("tera", "T", scientific 1 12),
    ("peta", "P", scientific 1 15),
    ("exa", "E", scientific 1 18)
  ]
