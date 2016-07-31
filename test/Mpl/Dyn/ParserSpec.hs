module Mpl.Dyn.ParserSpec where

import Test.Hspec
import Mpl.Dyn.AST          (AST(..))
import Mpl.Dyn.Parser       (ParseType(..), parseFile)
import Text.Trifecta.Result (Result(Success, Failure))

test name filename parseType expectedResult = it name $ do
  result <- parseFile parseType ("test/TestCases/Dyn/Parser/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

spec :: Spec
spec = do
  describe "integer" $ do
    test "a single digit" "integer-00.mpldyn" Exp (AInt 1)
    test "several digits" "integer-01.mpldyn" Exp (AInt 1234567890)
    test "negative"       "integer-02.mpldyn" Exp (AInt (-1234567890))

  describe "real" $ do
    test "a single digit" "real-00.mpldyn" Exp (AReal 1.0)
    test "several digits" "real-01.mpldyn" Exp (AReal 1234567890.0)
    test "negative"       "real-02.mpldyn" Exp (AReal (-1234567890.0))

  describe "identifier" $ do
    test "constant"      "constant-00.mpldyn" Prog (AProg [ADef (ASym "myConst") (AInt 123)])
    test "two constants" "constant-01.mpldyn" Prog (AProg [ADef (ASym "myConst") (AInt 123), ADef (ASym "otherConst") (AInt 890)])
