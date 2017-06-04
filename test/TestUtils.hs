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
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import           Prelude                    hiding (Foldable)
import           Test.Hspec
import qualified Data.List                  as List
import qualified Mpl.Rendering              as Rendering
import qualified Mpl.Rendering.ParserError  as ParserError
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

expect :: Either Expectation Expectation -> Expectation
expect (Left a)  = a
expect (Right a) = a

isParseError :: String -> Rendering.Doc -> (ByteString, Either StatefulError a) -> Expectation
isParseError code expected (bs, Left e) = do
  let expectedString = Rendering.render expected
  let errorString    = ParserError.errorMessage bs e
  if List.isInfixOf expectedString errorString
    then
      return ()
    else do
      expectationFailure $ concat
        [ "\n"
        , "==== Expected " ++ show code ++ " to contain the string:\n\n"
        , expectedString
        , "\n\n"
        , "==== This was the error that was produced:\n\n"
        , errorString
        , "\n"
        ]

isParseError code _ _ =
  fail $ "Successfully parsed " ++ show code
