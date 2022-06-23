{-# LANGUAGE OverloadedStrings #-}

import Calc.Scalar as S
import Calc.Units as U
import Data.Map as M
import Test.Hspec

main :: IO ()
main = hspec $ do
  testUnits
  testScalars

testUnits =
  describe "units" $ do
    it "is a singleton unit" $ do
      U.singleton "cm" `shouldBe` Units (M.singleton "cm" 1)

testScalars =
  describe "Unitless scalar" $ do
    it "has no units" $ do
      (1 :: Scalar) `shouldBe` Scalar 1 Nothing
