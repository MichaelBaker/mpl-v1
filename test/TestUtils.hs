module TestUtils
  ( module TestUtils
  , Expectation
  , describe
  , it
  , shouldBe
  , readJs
  , expectationFailure
  ) where

import           Language.JavaScript.Parser (readJs)
import           Mpl.ParserUtils            (ParseResult)
import           Mpl.Prelude
import           Mpl.Rendering.ParserError  (errorMessage)
import           Mpl.Utils
import           Prelude                    hiding (Foldable)
import           Test.Hspec
import qualified V8

mkTransformsTo :: (Foldable b, Eq c, Show c) => (t -> ParseResult a) -> (Base b c -> c) -> (a -> b) -> t -> c -> IO ()
mkTransformsTo parseExpressionText discardAnnotation f text expected =
  case snd $ parseExpressionText text of
    Left e -> fail $ show e
    Right a -> (cata discardAnnotation $ f a) `shouldBe` expected

mkTranslatesToJS parseExpressionText translateToJS mplCode jsCode =
  case snd $ parseExpressionText mplCode of
    Left e -> fail $ show e
    Right a -> jsIR (translateToJS a) `shouldBe` jsIR (readJs jsCode)

evalJS jsCode = do
  V8.withContext $ \context -> do
    V8.eval context (stringToText jsCode)
