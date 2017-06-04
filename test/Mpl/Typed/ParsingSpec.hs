module Mpl.Typed.ParsingSpec where

import           Mpl.Prelude
import           Mpl.Rendering
import           Mpl.Rendering.ParserError
import           Mpl.Typed.TestUtils
import           Mpl.Utils
import           TestUtils
import qualified Data.List         as List
import qualified Mpl.Common.Syntax as CS
import qualified Mpl.Typed.Parsing as Parsing
import qualified Mpl.Typed.Syntax  as S

spec = do
  it "parses type annotations" $ do
    "a: Integer" `parsesTo` (typeAnnotation (symbol "a") (typeSymbol "Integer"))

  it "parses type application" $ do
    "a: -> Integer Integer" `parsesTo`
      (typeAnnotation
        (symbol "a")
        (typeApplication
          (typeSymbol "->")
          [(typeSymbol "Integer"), (typeSymbol "Integer")]))

  it "parses type annotations for subexpressions" $ do
    "f (a: Integer)" `parsesTo`
      (application
        (symbol "f")
        [typeAnnotation (symbol "a") (typeSymbol "Integer")])

  it "binds trailing type annotations to their closest expression" $ do
    "f a: Integer" `isSameAs` "f (a: Integer)"

  it "binds type annotations to their closest expression" $ do
    "f: Integer a" `isSameAs` "(f: Integer) a"

  it "binds trailing type annotations with spaces to their closest expression" $ do
    "f a : Integer" `isSameAs` "f (a: Integer)"

  it "binds type annotations with whitespace to their closest expression" $ do
    "f : Integer a" `isSameAs` "(f: Integer) a"

  it "parses function arguments without a type annotation" $ do
    "#(a = a)" `parsesTo`
      (function
        [binder "a"]
        (symbol "a"))

  it "parses function arguments with a type annotation" $ do
    "#(a: Integer = a)" `parsesTo`
      (function
        [annotatedBinder (binder "a") (typeSymbol "Integer")]
        (symbol "a"))

  it "parses multiple function arguments with type annotations" $ do
    "#(a: Integer b: Integer = a)" `parsesTo`
      (function
        [ annotatedBinder (binder "a") (typeSymbol "Integer")
        , annotatedBinder (binder "b") (typeSymbol "Integer")
        ]
        (symbol "a"))

  it "parses multiple function arguments with mixed annotations" $ do
    "#(a: Integer b c: Float = a)" `parsesTo`
      (function
        [ annotatedBinder (binder "a") (typeSymbol "Integer")
        , binder "b"
        , annotatedBinder (binder "c") (typeSymbol "Float")
        ]
        (symbol "a"))

  it "renders type annotation parse errors" $ do
    "a: 0nteger"   `errorContains` ("incorrect " <~> callout_ "type annotation")
    "f a: 0nteger" `errorContains` ("incorrect " <~> callout_ "type annotation")

  it "renders binder type annotation parse errors" $ do
    "#(a: 0nteger = a)" `errorContains` ("incorrect " <~> callout_ "type annotation")

  it "renders mixed binder type annotation parse errors" $ do
    "#(a: Integer b c: 1loat = a)" `errorContains` ("incorrect " <~> callout_ "type annotation")

errorContains code expected = do
   let result = Parsing.parseString code
   isParseError code expected result

isSameAs a b =
  case Parsing.parseString a of
    (bs, Left e) ->
      expectationFailure (errorMessage bs e)
    (_, Right resultA) ->
      case Parsing.parseString b of
        (bs, Left e) ->
          expectationFailure (errorMessage bs e)
        (_, Right resultB) -> do
          shouldBe
            (render $ cata pretty $ cata discardAnnotation resultA)
            (render $ cata pretty $ cata discardAnnotation resultB)

type FixType   = Fix S.Type
type FixBinder = Fix (S.Binder FixType)
type FixSyntax = Fix (S.SyntaxF FixType FixBinder)

parsesTo :: String -> FixSyntax -> Expectation
parsesTo code expected = expect $ do
  (_, result) <- stringToSyntax code
  result
    |> cata discardAnnotation
    |> (`shouldBe` expected)
    |> return

typeAnnotation a b  = Fix $ S.typeAnnotation a b
symbol              = Fix . S.symbol
application a b     = Fix $ S.application a b
function a b        = Fix $ S.function a b
binder              = Fix . S.binder
annotatedBinder a b = Fix $ S.annotatedBinder a b

typeSymbol          = Fix . S.typeSymbol
typeApplication a b = Fix $ S.typeApplication a b

discardAnnotation =
  Fix
  . S.mapCommon (CS.mapBinder discardBinderAnnotation)
  . S.mapType (cata Fix)

discardBinderAnnotation :: Parsing.SourceBinder -> FixBinder
discardBinderAnnotation =
  cata (Fix . S.mapBinderType (cata Fix))

