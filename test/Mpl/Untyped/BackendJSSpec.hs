module Mpl.Untyped.BackendJSSpec where

import Mpl.Untyped.Parsing   (parseExpressionText)
import Mpl.Untyped.BackendJS (translateToJS)
import TestUtils             (describe, it, shouldBe, mkTranslatesToJS, loadConfig, mkEvalsJSTo)

translatesToJS = mkTranslatesToJS parseExpressionText translateToJS

spec = do
  config <- loadConfig

  let evalsTo = mkEvalsJSTo config parseExpressionText translateToJS

  it "parses integers" $ do
    "1" `translatesToJS` "1"
    "1" `evalsTo` "1"

  it "parses symbols" $ do
    "f" `translatesToJS` "f"

  it "function application" $ do
    "f 1 2" `translatesToJS` "f(1)(2)"
