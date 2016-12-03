module V8Spec where

import TestUtils (describe, it, shouldBe)

import qualified V8

spec = do
  describe "evalJS" $ do
    it "executes Javascript code in a V8 context and returns the result" $ do
      result <- V8.withContext $ \context -> do
        V8.eval context "\"Hello\" + \" World\""
      result `shouldBe` "Hello World"
