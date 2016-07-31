module Mpl.Dyn.ParserSpec where

import Test.Hspec
import Mpl.Dyn.AST          (AST(..))
import Mpl.Dyn.Parser       (ParseType(..), parseFile)
import Text.Trifecta.Result (Result(Success, Failure))

test name filename parseType expectedResult = it name $ do
  result <- parseFile parseType ("test/TestCases/Dyn/Parser/" ++ filename)
  case result of
    Failure ex -> fail $ show ex
    Success a  -> a `shouldBe` expectedResult

spec :: Spec
spec = do
  describe "integer" $ do
    test "a single digit" "integer-00.mpldyn" Exp (AInt 1)
    test "several digits" "integer-01.mpldyn" Exp (AInt 1234567890)

  describe "identifier" $ do
    test "constant"      "constant-00.mpldyn" Prog (AProg [ADef (ASym "myConst") (AInt 123)])
    test "two constants" "constant-01.mpldyn" Prog (AProg [ADef (ASym "myConst") (AInt 123), ADef (ASym "otherConst") (AInt 890)])
