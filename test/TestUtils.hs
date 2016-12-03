module TestUtils
  ( module TestUtils
  , describe
  , it
  , shouldBe
  ) where

import Test.Hspec                 (Expectation, describe, it, shouldBe, runIO)
import Mpl.Common.ParsingUtils    (Result(Success, Failure))
import Mpl.Utils                  (lazyTextToString, jsIR, stringToText)
import Mpl.LLVMUtils              (llvmIR)
import Language.JavaScript.Parser (readJs)
import Control.Monad.IO.Class     (liftIO)
import System.Environment         (lookupEnv)

import qualified V8

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
