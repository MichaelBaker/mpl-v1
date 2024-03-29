module Mpl.Untyped.BackendJSSpec where

import           Mpl.Prelude
import           Mpl.Untyped.BackendJS
import           Mpl.Utils
import           TestUtils
import qualified Mpl.Untyped.Parsing      as Parsing
import qualified Mpl.Untyped.SyntaxToCore as SyntaxToCore

spec = do
  it "parses integers" $ do
    "1" `translatesToJS` "1"
    "1" `evalsTo` "1"

  it "parses strings" $ do
    "\"this is not a string\"" `translatesToJS` "\"this is not a string\""
    "\"this is not a string\"" `evalsTo` "this is not a string"

  it "parses symbols" $ do
    "f" `translatesToJS` "f"

  it "function application" $ do
    "f 1 2" `translatesToJS` "((f)(1))(2)"

  it "anonymous functions" $ do
    "#(a = a 1)" `translatesToJS` "function(a) { return (a)(1); }"

  it "anonymous functions with multiple parameters" $ do
    "#(a b = a 1 b)" `translatesToJS`
      "function(a) { return function(b) { return ((a)(1))(b); }; }"

  it "translates the '+' function into a builtin curried function" $ do
    "+ a b" `translatesToJS` "((JSCore[\"+\"])(a))(b)"

  it "emits syntactically correct Javascript for symbolic functions" $ do
    "~@ a b" `translatesToJS` "((mplVar0)(a))(b)"

  it "emits syntactically correct Javascript for symbolic functions" $ do
    "#(~@ = ~@ 1)" `translatesToJS` "function(mplVar0) { return (mplVar0)(1); }"

translatesToJS :: String -> String -> Expectation
translatesToJS code expected =
  case snd (Parsing.parseString code) of
    Left e ->
      fail (show e)
    Right syntax -> do
      let result =
            (syntax :: Parsing.AnnotatedSyntax)
            |> envcata SyntaxToCore.transform
            |> ( run
               . SyntaxToCore.runTransform
               . SyntaxToCore.runTransformBinder
               )
            |> (jsIR . translateToJS)
      result `shouldBe` jsIR (readJs expected)

evalsTo :: String -> Text -> Expectation
evalsTo code expected =
  case snd (Parsing.parseString code) of
    Left e ->
      fail (show e)
    Right syntax -> do
      result <-
        (syntax :: Parsing.AnnotatedSyntax)
        |> envcata SyntaxToCore.transform
        |> ( run
           . SyntaxToCore.runTransform
           . SyntaxToCore.runTransformBinder
           )
        |> (lazyTextToString . jsIR . translateToJS)
        |> evalJS
      result `shouldBe` expected
