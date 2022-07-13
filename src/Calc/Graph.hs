{-# LANGUAGE OverloadedStrings #-}

module Calc.Graph where

{-

convertUnits :: Units -> Units -> Maybe Scalar
convertUnits (Units from) (Units to) = msum convs
  where
    unitsFrom = M.toList from
    unitsTo = M.toList to

    -- units left to be converted
    unconvertedFrom = unitsFrom \\ unitsTo
    unconvertedTo = unitsTo \\ unitsFrom

    -- all possible combinations of units
    xs = [simplify $ fromList x | x <- tail $ subsequences unconvertedFrom]
    ys = [simplify $ fromList y | y <- tail $ subsequences unconvertedTo]

    -- try to convert between unit combinations
    convs = [conversionScale (Units from) (Units to) | (from, x) <- xs, (to, y) <- ys]

convert :: Scalar -> Units -> Either String Scalar
convert (Scalar x Nothing) to = Right $ Scalar x (Just to)
convert x@(Scalar _ (Just from)) to
  | from == to = Right x
  | otherwise = case convertUnits from to of
    Nothing -> Left "no convert"
    Just y -> convert (x * y) to
-}
