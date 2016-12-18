module TestUtils
  ( module TestUtils
  , describe
  , it
  , shouldBe
  ) where

import Control.Monad.IO.Class     (liftIO)
import Data.Functor.Foldable      (Base, Fix, Foldable)
import Language.JavaScript.Parser (readJs)
import Mpl.Annotation             (Fixed, discardAnnotation)
import Mpl.ParserUtils            (Parsed, Result(Success, Failure))
import Mpl.LLVMUtils              (llvmIR)
import Mpl.Utils                  (lazyTextToString, jsIR, stringToText)
import Mpl.SyntaxErrorMessage     (errorMessage)
import System.Environment         (lookupEnv)
import Test.Hspec                 (Expectation, describe, it, shouldBe, runIO, expectationFailure)

import Prelude hiding (Foldable)

import qualified V8

mkParsesTo :: (Eq (Fixed a), Show (Fixed a), Foldable a) => (t -> Result a) -> t -> Fixed a -> IO ()
mkParsesTo parseExpressionText text expected =
  case parseExpressionText text of
    Failure e -> fail $ show e
    Success a -> (discardAnnotation a) `shouldBe` expected

mkParserErrorsWith :: (Show t) => (t -> Result a) -> t -> String -> IO ()
mkParserErrorsWith parseExpressionText text expected =
  case parseExpressionText text of
    Failure e ->
      if errorMessage e == expected
        then return ()
        else do
          expectationFailure $ concat
            [ "\n"
            , "==== Expected " ++ show text ++ " to produce the error: \n\n"
            , expected
            , "\n\n"
            , "==== But this was the error that was produced:\n\n"
            , errorMessage e
            , "\n"
            ]
    Success a -> fail $ "Successfully parsed " ++ show text

mkIsSameAs parseExpressionText a b =
  case parseExpressionText a of
    Failure e  -> fail $ show e
    Success a' ->
      case parseExpressionText b of
        Failure e  -> fail $ show e
        Success b' -> (discardAnnotation a') `shouldBe` (discardAnnotation b')

mkTranslatesToJS parseExpressionText translateToJS mplCode jsCode =
  case parseExpressionText mplCode of
    Failure e -> fail $ show e
    Success a -> jsIR (translateToJS a) `shouldBe` jsIR (readJs jsCode)

mkTranslatesToLLVM parseExpressionText translateToLLVM mplCode expected = do
  case parseExpressionText mplCode of
    Failure e -> fail $ show e
    Success a -> do
      result <- llvmIR (translateToLLVM a)
      result `shouldBe` expected

data Config = Config
  { nodePath :: String -- The filepath to a node executable for running javascript programs
  } deriving (Show, Read)

loadConfig = runIO $ do
  configFilepath <- lookupEnv "TEST_CONFIG_FILE"

  case configFilepath of
    Nothing -> error "You must set the TEST_CONFIG_FILE environment variable when running tests. Look in `script/test` for an example."
    Just path -> readFile path >>= return . read :: IO Config

mkEvalsJSTo config parseExpressionText translateToJS mplCode expected =
  case parseExpressionText mplCode of
    Failure e -> fail $ show e
    Success a -> do
      let jsCode = lazyTextToString $ jsIR (translateToJS a)
      result <- V8.withContext $ \context -> do
        V8.eval context (stringToText jsCode)
      result `shouldBe` expected
