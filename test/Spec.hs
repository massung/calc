{-# LANGUAGE OverloadedStrings #-}

import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Scalar
import Calc.Scalar as S
import Calc.Units as U
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either.Extra
import Data.Map as M
import Test.Hspec
import Text.Parsec

noUnits = Units M.empty

epsilon = 1e-2

main :: IO ()
main = hspec $ do
  testUnits
  testScalars
  testConversions

testExpr s ans = it (unwords [s, "==", show ans]) $ eval `shouldBe` Right True
  where
    eval = do
      expr <- mapLeft ExprError $ parse exprParser "" s
      case evalState (runExceptT $ evalExpr expr) [] of
        Right x -> return $ abs (x - ans) < epsilon
        Left e -> return False

testUnits = do
  describe "units" $ do
    it "ft == ft" $ do
      ("ft" :: Units) == ("ft" :: Units) `shouldBe` True
    it "noUnits <> ft" $ do
      (noUnits <> "ft") `shouldBe` "ft"
    it "ft <> noUnits" $ do
      ("ft" <> noUnits) `shouldBe` "ft"
    it "ft <> ft" $ do
      ("ft" <> "ft") `shouldBe` ("ft^2" :: Units)
    it "ft <> s" $ do
      ("ft" <> "s") `shouldBe` ("ft s" :: Units)
    it "recipUnits ft" $ do
      recipUnits "ft" `shouldBe` ("ft^-1" :: Units)
    it "recipUnits ft^2" $ do
      recipUnits "ft^2" `shouldBe` ("ft^-2" :: Units)
    it "recipUnits ft^-2" $ do
      recipUnits "ft^-2" `shouldBe` ("ft^2" :: Units)
    it "divideUnits ft^2 ft" $ do
      divideUnits "ft^2" "ft" `shouldBe` ("ft" :: Units)
    it "divideUnits ft ft^2" $ do
      divideUnits "ft" "ft^2" `shouldBe` ("ft^-1" :: Units)
    it "divideUnits ft s" $ do
      divideUnits "ft" "s" `shouldBe` ("ft/s" :: Units)

testScalars = do
  describe "scalars" $ do
    it "no units" $ do
      (1 :: Scalar) `shouldBe` Scalar 1 noUnits
    it "simple units" $ do
      ("2 ft" :: Scalar) `shouldBe` Scalar 2 (fromUnit "ft")
    it "compound units" $ do
      ("2 ft/s" :: Scalar) `shouldBe` Scalar 2 (fromUnitList [("ft", 1), ("s", -1)])

testConversions = do
  describe "basic conversions" $ do
    testExpr "1 ft : in" "12 in"
    testExpr "12 in : ft" "1 ft"
    testExpr "1 ft : cm" "30.48 cm"
    testExpr "1 ft^2 : cm^2" "929.03 cm^2"

  describe "si conversions" $ do
    testExpr "1 aL : L" "1e-18 L"
    testExpr "1 fL : L" "1e-15 L"
    testExpr "1 pL : L" "1e-12 L"
    testExpr "1 nL : L" "1e-9 L"
    testExpr "1 uL : L" "1e-6 L"
    testExpr "1 mL : L" "1e-3 L"
    testExpr "1 cL : L" "1e-2 L"
    testExpr "1 dL : L" "1e-1 L"
    testExpr "1 daL : L" "1e1 L"
    testExpr "1 hL : L" "1e2 L"
    testExpr "1 kL : L" "1e3 L"
    testExpr "1 ML : L" "1e6 L"
    testExpr "1 GL : L" "1e9 L"
    testExpr "1 TL : L" "1e12 L"
    testExpr "1 PL : L" "1e15 L"
    testExpr "1 EL : L" "1e18 L"

  describe "multi-step conversions" $ do
    testExpr "1 cable : h" "1800 h"
    testExpr "1 gal : floz" "128 floz"

  describe "simplified conversions" $ do
    testExpr "1 ft^2 : in^2" "144 in^2"
    testExpr "1 m^3 : cm^3" "1000000 cm^3"

  describe "compound conversions" $ do
    testExpr "1 mi/hr : ft/s" "1.467 ft/s"

  describe "dimension conversions" $ do
    testExpr "1 ha : m^2" "10000 m^2"
    testExpr "2 N : kg m/s^2" "2 kg m/s^2"

  describe "complex dimension conversions" $ do
    testExpr "1 L : in^3" "61.02 in^3"
    testExpr "2 N : ft lb/min^2" "52077.696 ft lb/min^2"

  describe "simple expressions" $ do
    testExpr "1 + 2" 3
    testExpr "1 - 2" (-1)
    testExpr "1 * 2" 2
    testExpr "1 / 2" 0.5
    testExpr "1 + 2 ft" "3 ft"
    testExpr "2 ft + 1" "3 ft"
    testExpr "1 ft + 1 in" "13 in"
    testExpr "12 in + 1 ft" "2 ft"

  describe "harmonized expressions" $ do
    testExpr "1 ft * 2 in" "24 in^2"
    testExpr "12 in * 1 ft" "1 ft^2"
    testExpr "1 ft / 2 in" 6
    testExpr "1 ft * 2 in^-1" 24
    testExpr "1 ft * 2 in^2" "24 in^3"
