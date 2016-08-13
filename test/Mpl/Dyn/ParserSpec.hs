module Mpl.Dyn.ParserSpec where

import Test.Hspec
import Mpl.Dyn.AST          (AST(..), emptySpan)
import Mpl.Dyn.Parser       (ParseType(..), parseFile, parseString)
import Text.Trifecta.Result (Result(Success, Failure))

testFile name filename parseType expectedResult = it name $ do
  result <- parseFile parseType ("test/TestCases/Dyn/Parser/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

testString name string parseType expectedResult = it name $ do
  let result = parseString parseType string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

int     a   = AInt     a emptySpan
sym     a   = ASym     a emptySpan
real    a   = AReal    a emptySpan
prog    a   = AProg    a emptySpan
def     a b = ADef     a b emptySpan
lam     a b = ALam     a b emptySpan
recdefs a   = ARecDefs a emptySpan
rec     a   = ARec     a emptySpan
field   a b = AField   a b emptySpan
list    a   = AList    a emptySpan

spec :: Spec
spec = do
  describe "integer" $ do
    testFile "a single digit" "integer-00.mpldyn" Exp (int 1)
    testFile "several digits" "integer-01.mpldyn" Exp (int 1234567890)
    testFile "negative"       "integer-02.mpldyn" Exp (int (-1234567890))

  describe "real" $ do
    testFile "a single digit" "real-00.mpldyn" Exp (real 1.0)
    testFile "several digits" "real-01.mpldyn" Exp (real 1234567890.0)
    testFile "negative"       "real-02.mpldyn" Exp (real (-1234567890.0))

  describe "symbol" $ do
    testString "an alphabetic symbol" "abc" Exp (sym "abc")

  describe "definition" $ do
    testFile "constant"          "definition-00.mpldyn" Def (def (sym "myConst") (int 123))
    testFile "function constant" "definition-01.mpldyn" Def (def (sym "fun") (lam [] (real 2.0)))
    testFile "function sugar"    "definition-02.mpldyn" Def (def (sym "fun") (lam [sym "a", sym "b"] (real 3.0)))

  describe "lambda" $ do
    testFile "lambda with zero arguments"  "lambda-00.mpldyn" Exp (lam [] (int 9))
    testFile "lambda with one argument"    "lambda-01.mpldyn" Exp (lam [sym "a"] (int 9))
    testFile "lambda with three arguments" "lambda-02.mpldyn" Exp (lam [sym "a", sym "b", sym "c"] (int 9))

  describe "record" $ do
    testString "empty record"                   "{}"     Exp (rec [])
    testString "record with one symbolic field" "{a: 1}" Exp (rec [field (sym "a") (int 1)])
    testString "record with one numberic field" "{0: 1}" Exp (rec [field (int 0) (int 1)])
    testFile   "record with multiple fields and trailing comma" "record-00.mpldyn" Exp (rec [
      field (int 0) (int 1),
      field (sym "a") (int 3),
      field (sym "b") (sym "c")
      ])

  describe "list" $ do
    testString "empty list" "[]" Exp (list [])
    testFile "list with multiple elements and trailing comma" "list-00.mpldyn" Exp (list [
      (int 0),
      (rec [field (sym "a") (int 3)]),
      (real 32.1),
      (sym "a")
      ])

  describe "program" $ do
    testFile "two constants" "program-00.mpldyn" Prog (prog $ recdefs [
      def (sym "myConst")    (int 123),
      def (sym "otherConst") (int 890)
      ])

    testFile "function sugar" "program-01.mpldyn" Prog (prog $ recdefs [
      def (sym "f") (lam [sym "a", sym "b"] (int 123))
      ])

    testFile "two function sugar" "program-02.mpldyn" Prog (prog $ recdefs [
      def (sym "f") (lam [sym "a", sym "b"] (int 123)),
      def (sym "g") (lam [sym "a", sym "b"] (sym "a"))
      ])
