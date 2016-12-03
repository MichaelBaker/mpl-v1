module Mpl.Untyped.BackendLLVMSpec where

import Mpl.Untyped.Parsing     (parseExpressionText)
import Mpl.Untyped.BackendLLVM (translateToLLVM)
import TestUtils               (describe, it, shouldBe, mkTranslatesToLLVM)
import Data.List               (intercalate)

translatesToLLVM = mkTranslatesToLLVM parseExpressionText translateToLLVM

spec = do
  describe "BackendLLVM" $ do
    it "parses integers" $ do
      "2" `translatesToLLVM`
        intercalate "\n"
          [ "; ModuleID = 'Main'"
          , ""
          , "define i32 @main() {"
          , "_0:"
          , "  ret i32 2"
          , "}"
          , ""
          ]
