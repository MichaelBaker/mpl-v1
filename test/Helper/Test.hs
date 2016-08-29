module Helper.Test where

import Test.Hspec           (it, shouldBe, expectationFailure)
import Text.Trifecta.Result (Result(Success, Failure))

makeTestFile caseDir parseFile transform name filename parseType expectedResult = it name $ do
  result <- parseFile parseType (caseDir ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> transform a `shouldBe` expectedResult

makeTestString parseString transform name string parseType expectedResult = it name $ do
  let result = parseString parseType string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> transform a `shouldBe` expectedResult
