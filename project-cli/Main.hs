module Main where

import           Options.Generic
import           Turtle
import qualified Data.Text as Text

main :: IO ()
main = do
  options <- getRecord "./bin/cli"
  handle options

handle Build = do
  procs "stack" ["install"] mempty

handle Documentation = do
  procs "stack" ["haddock"] mempty

handle (Test { pattern = maybePattern }) = do
  case unHelpful maybePattern of
    Nothing ->
      procs "stack" ["test"] mempty
    Just pattern -> do
      let testArguments = Text.intercalate " " ["-m", pattern]
      procs "stack" ["test", "--test-arguments", testArguments] mempty

data Options
  = Build
  | Documentation
  | Test
    { pattern :: Maybe Text.Text <?> "Only run tests with names matching this pattern." }
  deriving (Show, Generic)

instance ParseRecord Options
