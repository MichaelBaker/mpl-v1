module Mpl.ParserSpec where

import Test.Hspec

import Mpl.Parser (AST(..), parse)

spec :: Spec
spec = do
  describe "grammar" $ do
    it "parses a single number" $ do
      let (parses, _) = parse ["1"]
      parses `shouldBe` [Int "1"]
