module Mpl.ParserSpec where

import Test.Hspec

import Mpl.Parser (AST(..), parse)

spec :: Spec
spec = do
  describe "grammar" $ do
    it "parses a single number" $ do
      let (parses, _) = parse "12"
      parses `shouldBe` [Int "12"]

    it "parses prefix function application" $ do
      let (parses, r) = parse "(f 92)"
      parses `shouldBe` [App (Ident "f") (Int "92")]

