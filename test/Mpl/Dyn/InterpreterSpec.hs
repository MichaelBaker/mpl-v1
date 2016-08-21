module Mpl.Dyn.InterpreterSpec where

import Test.Hspec
import Mpl.Dyn.AST          (AST(..), emptySpan)
import Mpl.Dyn.Parser       (ParseType(Exp), parseFile, parseString)
import Mpl.Dyn.Interpreter  (interpret)
import Text.Trifecta.Result (Result(Success, Failure))

testFile name filename expectedResult = it name $ do
  result <- parseFile Exp ("test/TestCases/Dyn/Interpreter/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> interpret a `shouldBe` expectedResult

testString name string expectedResult = it name $ do
  let result = parseString Exp string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> interpret a `shouldBe` expectedResult

-- TODO
-- Add a test for closures
-- Calling something that isn't a function

spec :: Spec
spec = do
  describe "integer" $ do
    testString "simple integer" "234" "234"

  describe "real" $ do
    testString "simple real" "234.123" "234.123"

  describe "symbol" $ do
    testString "undefined symbol" "a" "\"<Undefined symbol 'a'>\""

  describe "list" $ do
    testString "list of ints" "[1,2,3]" "[1, 2, 3]"

  describe "record" $ do
    testString "two field record" "{ 1: \"a\", wat: \"yep\" }" "{wat: \"yep\", 1: \"a\"}"

  describe "utf16" $ do
    testString "simple utf16 string" "\"hello\"" "\"hello\""

  describe "let" $ do
    testString "simple let" "let a = 5 in a" "5"
    testFile   "two bindings" "let-00.mpldyn" "6"

  describe "lambda" $ do
    testString "simple thunk" "(# 5)" "(# 5)"
    testString "simple lambda" "(# a = a)" "(# a = a)"

  describe "application" $ do
    testString "simple application" "let f a = a in f 5" "5"
