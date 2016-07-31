module Mpl.Dyn.ParserSpec where

import Test.Hspec
import Mpl.Dyn.AST          (AST(..), emptySpan)
import Mpl.Dyn.Parser       (ParseType(..), parseFile)
import Text.Trifecta.Result (Result(Success, Failure))

test name filename parseType expectedResult = it name $ do
  result <- parseFile parseType ("test/TestCases/Dyn/Parser/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

int  a   = AInt  a emptySpan
sym  a   = ASym  a emptySpan
real a   = AReal a emptySpan
prog a   = AProg a emptySpan
def  a b = ADef  a b emptySpan
lam  a b = ALam  a b emptySpan

spec :: Spec
spec = do
  describe "integer" $ do
    test "a single digit" "integer-00.mpldyn" Exp (int 1)
    test "several digits" "integer-01.mpldyn" Exp (int 1234567890)
    test "negative"       "integer-02.mpldyn" Exp (int (-1234567890))

  describe "real" $ do
    test "a single digit" "real-00.mpldyn" Exp (real 1.0)
    test "several digits" "real-01.mpldyn" Exp (real 1234567890.0)
    test "negative"       "real-02.mpldyn" Exp (real (-1234567890.0))

  describe "identifier" $ do
    test "constant"      "constant-00.mpldyn" Prog (prog [def (sym "myConst") (int 123)])
    test "two constants" "constant-01.mpldyn" Prog (prog [
      def (sym "myConst")    (int 123),
      def (sym "otherConst") (int 890)
      ])
    test "function constant" "constant-02.mpldyn" Prog (prog [def (sym "fun") (lam [] (real 2.0))])

  describe "lambda" $ do
    test "lambda with zero arguments"  "lambda-00.mpldyn" Exp (lam [] (int 9))
    test "lambda with one argument"    "lambda-01.mpldyn" Exp (lam [sym "a"] (int 9))
    test "lambda with three arguments" "lambda-02.mpldyn" Exp (lam [sym "a", sym "b", sym "c"] (int 9))
