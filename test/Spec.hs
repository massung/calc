{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Calc.Defs
import Calc.Dims
import Calc.Error
import Calc.Eval
import Calc.Parser.Expr
import Calc.Parser.Scalar
import Calc.Parser.Units
import Calc.Scalar as S
import Calc.Script
import Calc.Units as U
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either.Extra
import Data.Map as M
import Test.Hspec
import Text.Parsec

noUnits :: Units
noUnits = Units M.empty

epsilon = 1e-2

main :: IO ()
main = do
  defs <- builtInDefs
  hspec $ do
    testUnits
    testDims
    testScalars
    testConversions
    testArgs
    testDefs defs

testExprBasic defs args s ans = it (unwords [s, "==", show ans]) $ eval `shouldBe` Right True
  where
    eval = do
      expr <- mapLeft ExprError $ runParser exprParser defs "" s
      case evalState (runExceptT $ evalExpr expr) args of
        Right x -> return $ abs (x - ans) < epsilon
        Left e -> return False

testExpr = testExprBasic mempty []

testExprArgs = testExprBasic mempty

testScriptExpr defs = testExprBasic defs []

testDims = do
  describe "baseDims" $ do
    it "ft == Length" $ do
      dims "ft" == Dims [(Length, 1)]
    it "ft/s == Speed" $ do
      dims "ft/s" == baseDims Speed

testUnits = do
  describe "IsString units" $ do
    it "ft == ft" $ do
      ("ft" :: Units) `shouldBe` ("ft" :: Units)

  describe "parsing units" $ do
    it "ft == ft" $ do
      parseUnits "ft" `shouldBe` Right "ft"
    it "ft/s == ft/s" $ do
      parseUnits "ft/s" `shouldBe` Right "ft/s"
    it "asdf == invalid units" $ do
      parseUnits "asdf" `shouldSatisfy` isLeft

  describe "append units" $ do
    it "noUnits <> ft" $ do
      (noUnits <> "ft") `shouldBe` "ft"
    it "ft <> noUnits" $ do
      ("ft" <> noUnits) `shouldBe` "ft"
    it "ft <> ft" $ do
      ("ft" <> "ft") `shouldBe` ("ft^2" :: Units)
    it "ft <> s" $ do
      ("ft" <> "s") `shouldBe` ("ft s" :: Units)

  describe "map units" $ do
    it "recipUnits ft" $ do
      recipUnits "ft" `shouldBe` ("ft^-1" :: Units)
    it "recipUnits ft^2" $ do
      recipUnits "ft^2" `shouldBe` ("ft^-2" :: Units)
    it "recipUnits ft^-2" $ do
      recipUnits "ft^-2" `shouldBe` ("ft^2" :: Units)
    it "ft^2 </> ft" $ do
      "ft^2" </> "ft" `shouldBe` ("ft" :: Units)
    it "ft </> ft^2" $ do
      "ft" </> "ft^2" `shouldBe` ("ft^-1" :: Units)
    it "ft </> s" $ do
      "ft" </> "s" `shouldBe` ("ft/s" :: Units)

testScalars = do
  describe "scalars" $ do
    it "no units" $ do
      (1 :: Scalar) `shouldBe` Scalar 1 mempty mempty

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
    testExpr "1 cable : h" "2160 h"
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
    testExpr "2 N : ft lb/min^2" "52077.69973 ft lb/min^2"

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
    testExpr "2 ft * 3" "6 ft"
    testExpr "2 * 3 ft" "6 ft"
    testExpr "1 ft * 2 in" "24 in^2"
    testExpr "12 in * 1 ft" "1 ft^2"
    testExpr "1 ft / 2 in" 6
    testExpr "1 ft * 2 in^-1" 24
    testExpr "1 ft * 2 in^2" "24 in^3"
    testExpr "1 m/s * 1 min" "60 m"

  describe "precision loss" $ do
    testExpr "((2 ft : in) : ft) : in" "24 in"

  describe "angle conversions" $ do
    testExpr "1 rad : deg" "57.3 deg"
    testExpr "1 rad : turn" "0.1592 turn"
    testExpr "1 rad : rev" "0.1592 rev"

  describe "area conversions" $ do
    testExpr "1 ha : m^2" "10000 m^2"
    testExpr "1 ha : acre" "2.471 acre"

  describe "duration conversions" $ do
    testExpr "1 s : min" "0.01667 min"
    testExpr "1 s : hr" "2.778e-4 hr"
    testExpr "1 s : day" "1.157e-5 day"

testArgs = do
  describe "single arguments" $ do
    testExprArgs [1] "_ ft" "1 ft"
    testExprArgs [1] "_ ft : in" "12 in"
    testExprArgs [2] "_ yd : ft" "6 ft"
    testExprArgs [1] "5 ft + _ in" "61 in"

  describe "multiple arguments" $ do
    testExprArgs [1, 2] "_ ft + _ in" "14 in"
    testExprArgs [2, 3, 1] "_ * _ - _" "5"

testDefs defs = do
  describe "code functions" $ do
    testScriptExpr defs "[abs -8 ft]" "8 ft"
    testScriptExpr defs "[signum -4 ft]" "-1 ft"
    testScriptExpr defs "[sqrt 4 ft^2]" "2 ft"
    testScriptExpr defs "[exp [log 16]]" 16
    testScriptExpr defs "[truncate 4.4 in]" "4 in"
    testScriptExpr defs "[floor 4.7 mi]" "4 mi"
    testScriptExpr defs "[ceil 4.2 kn]" "5 kn"
    testScriptExpr defs "[round 4.3 acre]" "4 acre"
    testScriptExpr defs "[sin 90 deg]" 1
    testScriptExpr defs "[cos [pi]]" (-1)
    testScriptExpr defs "[asin [sin 45 deg]] to deg" "45 deg"
    testScriptExpr defs "[asinh [sinh 1.4 rad]]" "1.4 rad"

  describe "script functions" $ do
    testScriptExpr defs "[areaOfCircle 4 m]" "50.266 m^2"
    testScriptExpr defs "[areaOfRect 2 ft; 3 in]" "72 in^2"
    testScriptExpr defs "[transferRate 10 GB; 1 hr]" "2.844 MB/s"
