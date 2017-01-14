module Mpl.Typed.BackendJSSpec where

import Mpl.Typed.Parsing   (parseExpressionText)
import Mpl.Typed.BackendJS (translateToJS)
import TestUtils           (describe, it, shouldBe, mkTranslatesToJS)

translatesToJS = mkTranslatesToJS parseExpressionText translateToJS

spec = do
  it "parses type annotations into comments" $ do
    "f (a: Integer): Function" `translatesToJS` "f( /* Integer */ a /* Function */ )"

  it "handles symbolic symbols" $ do
    "f (<>: Integer): Function" `translatesToJS` "f( /* Integer */ mplVar0 /* Function */ )"

  it "handles symbolic function parameters" $ do
    "#(<> = <>): Function" `translatesToJS` "function(mplVar0 /* Function */ ) { return mplVar0; }"
