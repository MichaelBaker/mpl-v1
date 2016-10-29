module TestUtils
  ( module TestUtils
  , describe
  , it
  , shouldBe
  ) where

import Test.Hspec              (describe, it, shouldBe)
import Mpl.Common.ParsingUtils (Result(Success, Failure))

mkParsesTo parseExpressionText text expected =
  case parseExpressionText text of
    Success a -> a `shouldBe` expected
    Failure e -> fail $ show e

mkIsSameAs parseExpressionText a b =
  case parseExpressionText a of
    Failure e  -> fail $ show e
    Success a' ->
      case parseExpressionText b of
        Failure e  -> fail $ show e
        Success b' -> a' `shouldBe` b'
