module Mpl.Common.SyntaxToCoreSpec where

import           Mpl.Prelude
import           Mpl.Utils
import           TestUtils
import qualified Mpl.Common.Core         as C
import qualified Mpl.Common.Parsing      as Parsing
import qualified Mpl.Common.SyntaxToCore as SyntaxToCore
import qualified Mpl.Parser              as Parser
import qualified Mpl.ParserUtils         as ParserUtils

spec = do
  it "converts integers" $ do
    "1" `transformsTo` (int 1)

  it "converts strings" $ do
    "\"this is not a string\"" `transformsTo` (utf8String "this is not a string")

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

transformsTo :: String -> Fix (C.CoreF (Fix C.Binder)) -> Expectation
transformsTo code expected =
  case Parser.parseByteString Parsing.parser ParserUtils.zeroDelta (stringToByteString code) mempty of
    Left e -> fail $ show e
    Right syntax -> do
      let result =
            (syntax :: ParserUtils.SourceAnnotated Parsing.Syntax)
            |> envcata SyntaxToCore.transform
            |> ( run
               . SyntaxToCore.runTransform
               . SyntaxToCore.runTransformBinder
               )
            |> cata (Fix . C.mapBinder (cata Fix))
      result `shouldBe` expected

int              = Fix . C.int
utf8String       = Fix . C.utf8String
symbol           = Fix . C.symbol
binder           = Fix . C.binder
application a b  = Fix $ C.application a b
function a b     = Fix $ C.function a b

