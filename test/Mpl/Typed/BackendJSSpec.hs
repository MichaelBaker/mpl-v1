module Mpl.Typed.BackendJSSpec where


import           Mpl.Prelude
import           Mpl.Typed.BackendJS
import           Mpl.Typed.TestUtils
import           Mpl.Utils
import           TestUtils
import qualified Mpl.Typed.SyntaxToCore as SyntaxToCore

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

translatesToJS :: String -> String -> Expectation
translatesToJS code expected = expect $ do
  (_, result) <- stringToCore code
  result
    |> (jsIR . translateToJS)
    |> (`shouldBe` jsIR (readJs expected))
    |> return

evalsTo :: String -> Text -> Expectation
evalsTo code expected = expect $ do
  (_, coreResult) <- stringToCore code
  return $ do
    result <-
      coreResult
        |> translateToJS
        |> jsIR
        |> lazyTextToString
        |> evalJS
    result `shouldBe` expected

