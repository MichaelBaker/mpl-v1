module TestUtils
  ( module TestUtils
  , Expectation
  , describe
  , it
  , shouldBe
  , readJs
  , expectationFailure
  ) where

import           Data.Functor.Foldable      (Base, Fix, Foldable)
import           Data.List                  (isInfixOf)
import           Language.JavaScript.Parser (readJs)
import           Mpl.ParserUtils            (ParseResult)
import           Mpl.Prelude
import           Mpl.Rendering.ParserError  (errorMessage)
import           Mpl.Utils                  (jsIR, cata)
import           Prelude                    hiding (Foldable)
import           Test.Hspec
import qualified V8

mkTransformsTo :: (Foldable b, Eq c, Show c) => (t -> ParseResult a) -> (Base b c -> c) -> (a -> b) -> t -> c -> IO ()
mkTransformsTo parseExpressionText discardAnnotation f text expected =
  case snd $ parseExpressionText text of
    Left e -> fail $ show e
    Right a -> (cata discardAnnotation $ f a) `shouldBe` expected

mkParserErrorContains :: (Show t) => (t -> ParseResult a) -> t -> String -> IO ()
mkParserErrorContains parseExpressionText text expected =
  case parseExpressionText text of
    (bs, Left e) ->
      if expected `isInfixOf` errorMessage bs e
        then return ()
        else do
          expectationFailure $ concat
            [ "\n"
            , "==== Expected " ++ show text ++ " to contain the string:\n\n"
            , expected
            , "\n\n"
            , "==== This was the error that was produced:\n\n"
            , errorMessage bs e
            , "\n"
            ]
    (_, Right a) -> fail $ "Successfully parsed " ++ show text

mkTranslatesToJS parseExpressionText translateToJS mplCode jsCode =
  case snd $ parseExpressionText mplCode of
    Left e -> fail $ show e
    Right a -> jsIR (translateToJS a) `shouldBe` jsIR (readJs jsCode)

evalJS jsCode = do
  V8.withContext $ \context -> do
    V8.eval context (stringToText jsCode)
