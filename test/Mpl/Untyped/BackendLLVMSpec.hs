module Mpl.Untyped.BackendLLVMSpec where

import Mpl.Untyped.Parsing     (parseExpressionText)
import Mpl.Untyped.BackendLLVM (translateToLLVM)
import TestUtils               (describe, it, shouldBe, mkTranslatesToLLVM)

translatesToLLVM = mkTranslatesToLLVM parseExpressionText translateToLLVM

spec = do
  describe "BackendLLVM" $ do
    it "parses integers" $ do
      "1" `translatesToLLVM` "wat"
