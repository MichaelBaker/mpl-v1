module Mpl.Typed.SyntaxToCoreSpec where

import           Mpl.ParserUtils        hiding (symbol)
import           Mpl.Prelude
import           Mpl.Typed.TestUtils
import           Mpl.Utils
import           TestUtils
import qualified Mpl.Common.Core        as CC
import qualified Mpl.Typed.Core         as C

type FixCore =
  Fix (C.CoreF FixType FixBinder)

type FixBinder =
  Fix (C.Binder FixType)

type FixType =
  Fix C.Type

type SourceBinder =
  SourceAnnotated (C.Binder SourceType)

type SourceType =
  SourceAnnotated C.Type

transformsTo :: String -> FixCore -> Expectation
transformsTo code expected =
  case snd (stringToCore code) of
    Left e ->
      fail $ show e
    Right result -> do
      result
      |> cata discardAnnotation
      |> (`shouldBe` expected)

discardAnnotation =
  Fix
  . C.mapBinder discardBinderAnnotation
  . C.mapType (cata Fix)

discardBinderAnnotation :: SourceBinder -> FixBinder
discardBinderAnnotation =
  cata (Fix . C.mapBinderType (cata Fix))

int =
  Fix . C.Common . CC.int

symbol =
  Fix . C.Common . CC.symbol

binder =
  Fix . C.CommonBinder . CC.binder

annotatedBinder name typeName =
  Fix $ C.AnnotatedBinder
          (binder name)
          (Fix $ C.TypeSymbol typeName)

application a b =
  Fix $ C.Common $ CC.application a b

function a b =
  Fix $ C.Common $ CC.function a b

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

  it "converts annotated binders" $ do
    "#(a: Integer = a)" `transformsTo`
      (function (annotatedBinder "a" "Integer") (symbol "a"))
