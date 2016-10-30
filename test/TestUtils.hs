module TestUtils
  ( module TestUtils
  , describe
  , it
  , shouldBe
  ) where

import Test.Hspec                 (describe, it, shouldBe)
import Mpl.Common.ParsingUtils    (Result(Success, Failure))
import Language.JavaScript.Parser (readJs, renderToText)

mkParsesTo parseExpressionText text expected =
  case parseExpressionText text of
    Failure e -> fail $ show e
    Success a -> a `shouldBe` expected

mkIsSameAs parseExpressionText a b =
  case parseExpressionText a of
    Failure e  -> fail $ show e
    Success a' ->
      case parseExpressionText b of
        Failure e  -> fail $ show e
        Success b' -> a' `shouldBe` b'

mkTranslatesToJS parseExpressionText translateToJS mplCode jsCode =
  case parseExpressionText mplCode of
    Failure e -> fail $ show e
    Success a -> renderToText (translateToJS a) `shouldBe` renderToText (readJs jsCode)
