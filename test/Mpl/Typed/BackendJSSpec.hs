module Mpl.Typed.BackendJSSpec where


import           Mpl.Prelude
import           Mpl.Typed.BackendJS
import           Mpl.Typed.Parsing
import           Mpl.Utils
import           TestUtils
import qualified Mpl.Typed.SyntaxToCore as SyntaxToCore

translatesToJS :: Text -> String -> Expectation
translatesToJS text expected =
  case snd $ parseExpressionText text of
    Left e -> fail $ show e
    Right syntax -> do
      let result =
            syntax
            |> envcata SyntaxToCore.transform
            |> ( run
               . SyntaxToCore.runTransform
               . SyntaxToCore.runTransformBinder
               )
            |> (jsIR . translateToJS)
      result `shouldBe` jsIR (readJs expected)

evalsTo :: Text -> Text -> Expectation
evalsTo text expected =
  case snd $ parseExpressionText text of
    Left e -> fail $ show e
    Right syntax -> do
      result <-
        syntax
        |> envcata SyntaxToCore.transform
        |> ( run
           . SyntaxToCore.runTransform
           . SyntaxToCore.runTransformBinder
           )
        |> (lazyTextToString . jsIR . translateToJS)
        |> evalJS
      result `shouldBe` expected

spec = do
  it "parses type annotations into comments" $ do
    "f (a: Integer): Function" `translatesToJS` "(f)( /* Integer */ a /* Function */ )"

  it "handles symbolic symbols" $ do
    "f (<>: Integer): Function" `translatesToJS` "(f)( /* Integer */ mplVar0 /* Function */ )"

  it "handles symbolic function parameters" $ do
    "#(<> = <>): Function" `translatesToJS` "function(mplVar0 /* Function */ ) { return mplVar0; }"

  it "handles annotated binders" $ do
    "#(a: Integer = a)" `translatesToJS`
      "function( /* Integer */ a) { return a; }"
    "#(a: Integer = a) 1" `evalsTo`
      "1"
