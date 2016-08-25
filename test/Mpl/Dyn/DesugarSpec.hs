module Mpl.Dyn.DesugarSpec where

import Test.Hspec
import Mpl.Span             (emptySpan)
import Mpl.Dyn.Core         (Core(..))
import Mpl.Dyn.Parser       (ParseType(Exp), parseFile, parseString)
import Mpl.Dyn.Desugar      (desugar)
import Text.Trifecta.Result (Result(Success, Failure))

testFile name filename expectedResult = it name $ do
  result <- parseFile Exp ("test/TestCases/Dyn/Desugar/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> desugar a `shouldBe` expectedResult

testString name string expectedResult = it name $ do
  let result = parseString Exp string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> desugar a `shouldBe` expectedResult

sym a = CSym a emptySpan

spec :: Spec
spec = do
  describe "symbol" $ do
    testString "simple symbol" "a" (sym "a")
