module Mpl.Typed.ParsingSpec where

import Mpl.Typed.Parsing ()
import Test.Hspec (describe, it, shouldBe)

spec = do
  describe "Parsing" $ do
    it "sanity checks" $ do
      1 `shouldBe` 1
