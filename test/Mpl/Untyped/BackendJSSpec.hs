module Mpl.Untyped.BackendJSSpec where

import Mpl.Untyped.Parsing   (parseExpressionText)
import Mpl.Untyped.Core      (syntaxToCore)
import Mpl.Untyped.BackendJS (translateToJS)
import Mpl.Utils             (envcata)
import TestUtils             (describe, it, shouldBe, mkTranslatesToJS, loadConfig, mkEvalsJSTo)

translatesToJS =
  mkTranslatesToJS
    parseExpressionText
    (translateToJS . (envcata syntaxToCore))

spec = do
  config <- loadConfig

  let evalsTo =
        mkEvalsJSTo
          config
          parseExpressionText
          (translateToJS . (envcata syntaxToCore))

  it "parses integers" $ do
    "1" `translatesToJS` "1"
    "1" `evalsTo` "1"

  it "parses symbols" $ do
    "f" `translatesToJS` "f"

  it "function application" $ do
    "f 1 2" `translatesToJS` "f(1)(2)"

  it "anonymous functions" $ do
    "#(a = a 1)" `translatesToJS` "function(a) { return a(1); }"

  it "anonymous functions with multiple parameters" $ do
    "#(a b = a 1 b)" `translatesToJS`
      "function(a) { return function(b) { return a(1)(b); }; }"

  it "translates the '+' function into a builtin curried function" $ do
    "+ a b" `translatesToJS` "JSCore[\"+\"](a)(b)"

  it "emits syntactically correct Javascript for symbolic functions" $ do
    "~@ a b" `translatesToJS` "mplVar0(a)(b)"

  it "emits syntactically correct Javascript for symbolic functions" $ do
    "#(~@ = ~@ 1)" `translatesToJS` "function(mplVar0) { return mplVar0(1); }"
