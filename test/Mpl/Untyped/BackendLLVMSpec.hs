module Mpl.Untyped.BackendLLVMSpec where

import Mpl.Untyped.Parsing     (parseExpressionText)
import Mpl.Untyped.BackendLLVM (translateToLLVM)
import TestUtils               (describe, it, shouldBe, mkTranslatesToLLVM)
import Data.List               (intercalate)

translatesToLLVM = mkTranslatesToLLVM parseExpressionText translateToLLVM

spec = do
  describe "BackendLLVM" $ do
    it "parses integers" $ do
      "f a 2 b" `translatesToLLVM`
        intercalate "\n"
          [ "; ModuleID = 'Main'"
          , ""
          , "define void @main() {"
          , "  %_2 = load i32 %_a"
          , "  %_3 = and 2, 1"
          , "  %_4 = load i32 %_b"
          , "  call i32 @f(i32 %_2, i32 %_3, i32 %_4)"
          , "  ret"
          , "}"
          ]
