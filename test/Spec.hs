{-# LANGUAGE OverloadedStrings #-}

import Calc.Scalar as S
import Calc.Units as U
import Data.Map as M
import Test.Hspec

main :: IO ()
main = hspec $ do
  testUnits
  testScalars

cm = U.singleton "cm"

miles = U.singleton "mi"

hours = U.singleton "hr"

mph = U.fromList [("mi", 1), ("hr", -1)]

testUnits =
  describe "build units" $ do
    it "singleton unit" $ do
      cm `shouldBe` Units (M.singleton "cm" 1)
    it "ratio unit" $ do
      mph `shouldBe` Units (M.fromList [("mi", 1), ("hr", -1)])
    it "reciprocal numerator" $ do
      recipUnits cm `shouldBe` Units (M.singleton "cm" (-1))
    it "reciprocal ratio" $ do
      recipUnits mph `shouldBe` Units (M.fromList [("mi", -1), ("hr", 1)])
    it "multiply units" $ do
      multiplyUnits cm cm `shouldBe` Units (M.singleton "cm" 2)
    it "divide units" $ do
      divideUnits miles hours `shouldBe` Units (M.fromList [("mi", 1), ("hr", -1)])
    it "simplify units" $ do
      multiplyUnits mph hours `shouldBe` miles
      divideUnits mph miles `shouldBe` recipUnits hours

testScalars =
  describe "Unitless scalar" $ do
    it "has no units" $ do
      (1 :: Scalar) `shouldBe` Scalar 1 Nothing
