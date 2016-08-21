module Mpl.Dyn.InterpreterSpec where

import Test.Hspec
import Mpl.Dyn.AST          (AST(..), emptySpan)
import Mpl.Dyn.Parser       (ParseType(Exp), parseFile, parseString)
import Mpl.Dyn.Interpreter  (interpret)
import Text.Trifecta.Result (Result(Success, Failure))

testFile name filename expectedResult = it name $ do
  result <- parseFile Exp ("test/TestCases/Dyn/Parser/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> interpret a `shouldBe` expectedResult

testString name string expectedResult = it name $ do
  let result = parseString Exp string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> interpret a `shouldBe` expectedResult

spec :: Spec
spec = do
  describe "integer" $ do
    testString "simple integer" "234" "234"

  describe "real" $ do
    testString "simple real" "234.123" "234.123"

  describe "symbol" $ do
    testString "undefined symbol" "a" "Undefined symbol 'a'"

  describe "list" $ do
    testString "list of ints" "[1,2,3]" "[1, 2, 3]"
