module Mpl.ParserSpec where

import Test.Hspec

import qualified Data.Text as T
import Mpl.Parser (AST(..), parse)

spec :: Spec
spec = do
  describe "grammar" $ do
    it "parses a single number" $ do
      let (parses, _) = parse ["1"]
      parses `shouldBe` [Int "1"]

    it "parses prefix function application" $ do
      let (parses, r) = parse $ T.words "f 1 )"
      parses `shouldBe` [App "f" (Int "1")]

