{-# LANGUAGE OverloadedStrings #-}

import Calc.Dim
import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Scalar
import Calc.Scalar as S
import Calc.Units as U
import Data.Either.Extra
import Data.Map as M
import Test.Hspec
import Text.Parsec

noUnits = Units M.empty

main :: IO ()
main = hspec $ do
  testUnits
  testScalars
  testConversions

testConv s ans =
  it s $ do
    eval `shouldBe` Right True
  where
    eval = do
      expr <- mapLeft ExprError $ parse exprParser "" s
      case evalExpr ans expr of
        Right (Term x) -> return True
        _ -> return False

testUnits =
  describe "units" $ do
    it "dims ft" $ do
      dims "ft" `shouldBe` Dims (M.singleton Length 1)
    it "dims ft/s" $ do
      dims "ft/s" `shouldBe` Dims (M.fromList [(Length, 1), (Duration, -1)])
    it "dims' ft/s" $ do
      dims' "ft/s" `shouldBe` M.fromList [(Length, "ft"), (Duration, "s^-1")]
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

testScalars =
  describe "scalars" $ do
    it "no units" $ do
      (1 :: Scalar) `shouldBe` Scalar 1 noUnits
    it "simple units" $ do
      ("2 ft" :: Scalar) `shouldBe` Scalar 2 (fromUnit "ft")
    it "compound units" $ do
      ("2 ft/s" :: Scalar) `shouldBe` Scalar 2 (fromUnitList [("ft", 1), ("s", -1)])

testConversions =
  describe "conversions" $ do
    testConv "1 + 2 ft" "3 ft"
    testConv "2 + 1 ft" "3 ft"
    testConv "1 ft : in" "12 in"
    testConv "12 in : ft" "1 ft"
    testConv "1 ft : cm" "30.48 cm"
