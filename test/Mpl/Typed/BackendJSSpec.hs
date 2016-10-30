module Mpl.Typed.BackendJSSpec where

import Mpl.Typed.Parsing   (parseExpressionText)
import Mpl.Typed.BackendJS (translateToJS)
import TestUtils           (describe, it, shouldBe, mkTranslatesToJS)

translatesToJS = mkTranslatesToJS parseExpressionText translateToJS

spec = do
  describe "BackendJS" $ do
    it "parses type annotations into comments" $ do
      "f (a: Integer): Function" `translatesToJS` "f( /* Integer */ a /* Function */ )"
