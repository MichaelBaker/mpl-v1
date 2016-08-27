module Mpl.Ty.ParserSpec where

import Test.Hspec
import Mpl.Span             (emptySpan)
import Mpl.Ty.AST           (AST(..), TyAST(..))
-- TODO: Pull ParseType into a shared module
import Mpl.Ty.Parser        (ParseType(..), parseFile, parseString)
import Text.Trifecta.Result (Result(Success, Failure))

import qualified Mpl.Dyn.AST as Dyn

testFile name filename parseType expectedResult = it name $ do
  result <- parseFile parseType ("test/TestCases/Ty/Parser/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

testString name string parseType expectedResult = it name $ do
  let result = parseString parseType string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

-- TODO: Pull helpers into a shared module
ann_exp a b = AAnnExp a b emptySpan
ty      a   = ATySym  a   emptySpan
int     a   = ADyn (Dyn.AInt a emptySpan)

spec :: Spec
spec = do
  describe "annotated expression" $ do
    testString "annotated int" "1 : Integer" Exp (ann_exp (int 1) (ty "Integer"))
