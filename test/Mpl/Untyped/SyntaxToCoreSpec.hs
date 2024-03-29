module Mpl.Untyped.SyntaxToCoreSpec where

import           Mpl.Prelude
import           Mpl.Utils
import           TestUtils
import qualified Mpl.Common.Core          as CC
import qualified Mpl.ParserUtils          as ParserUtils
import qualified Mpl.Untyped.Core         as C
import qualified Mpl.Untyped.Parsing      as Parsing
import qualified Mpl.Untyped.SyntaxToCore as SyntaxToCore

spec = do
  it "converts integers" $ do
    "1" `transformsTo` (int 1)

  it "converts symbols" $ do
    "a" `transformsTo` (symbol "a")

  it "converts functions of one parameter" $ do
    "#(a = a)" `transformsTo`
      (function (binder "a") (symbol "a"))

  it "curries functions of multiple parameters" $ do
    "#(a b c = a)" `transformsTo`
      (function (binder "a")
        (function (binder "b")
          (function (binder "c") (symbol "a"))))

  it "converts application of one argument" $ do
    "f 1" `transformsTo` (application (symbol "f") (int 1))

  it "curries application of multiple arguments" $ do
    "f 1 2 3" `transformsTo`
      (application
        (application
          (application
            (symbol "f")
            (int 1))
          (int 2))
        (int 3))

transformsTo :: String -> Fix (C.CoreF (Fix CC.Binder)) -> Expectation
transformsTo code expected =
  case snd (Parsing.parseString code) of
    Left e -> fail $ show e
    Right syntax -> do
      let result =
            (syntax :: Parsing.AnnotatedSyntax)
            |> envcata SyntaxToCore.transform
            |> ( run
               . SyntaxToCore.runTransform
               . SyntaxToCore.runTransformBinder
               )
            |> cata (Fix . C.mapBinder (cata Fix))
      result `shouldBe` expected

int              = Fix . C.Common . CC.int
symbol           = Fix . C.Common . CC.symbol
binder           = Fix . CC.binder
application a b  = Fix $ C.Common $ CC.application a b
function a b     = Fix $ C.Common $ CC.function a b
