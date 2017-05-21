module Mpl.Typed.ParsingSpec where

import           Mpl.Prelude
import           Mpl.Rendering
import           Mpl.Rendering.ParserError
import           Mpl.Utils
import           TestUtils
import qualified Data.List         as List
import qualified Mpl.Common.Syntax as CS
import qualified Mpl.Typed.Parsing as Parsing
import qualified Mpl.Typed.Syntax  as S

spec = do
  it "parses type annotations" $ do
    "a: Integer" `parsesTo` (typeAnnotation (symbol "a") (typeSymbol "Integer"))

  it "parses type annotations for subexpressions" $ do
    "f (a: Integer)" `parsesTo`
      (application
        (symbol "f")
        [typeAnnotation (symbol "a") (typeSymbol "Integer")])

  it "binds type annotations to their closest expression" $ do
    "f a: Integer" `isSameAs` "f (a: Integer)"

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
    "a: 0nteger"   `errorContains` ("incorrect " <~> callout_ "type")
    "f a: 0nteger" `errorContains` ("incorrect " <~> callout_ "type")

  it "renders binder type annotation parse errors" $ do
    "#(a: 0nteger = a)" `errorContains` ("incorrect " <~> callout_ "type")

  it "renders mixed binder type annotation parse errors" $ do
    "#(a: Integer b c: 1loat = a)" `errorContains` ("incorrect " <~> callout_ "type")

errorContains code expected =
  case Parsing.parseString code of
    (bs, Left e) ->
      if List.isInfixOf (render expected) (errorMessage bs e)
        then return ()
        else do
          expectationFailure $ concat
            [ "\n"
            , "==== Expected " ++ show code ++ " to contain the string:\n\n"
            , render expected
            , "\n\n"
            , "==== This was the error that was produced:\n\n"
            , errorMessage bs e
            , "\n"
            ]
    (_, Right a) ->
      fail $ "Successfully parsed " ++ show code

isSameAs a b =
  case snd (Parsing.parseString a) of
    Left e -> fail $ show e
    Right resultA ->
      case snd (Parsing.parseString b) of
        Left e ->
          fail $ show e
        Right resultB -> do
          (cata discardAnnotation resultA) `shouldBe` (cata discardAnnotation resultB)

type FixType   = Fix S.Type
type FixBinder = Fix (S.Binder FixType)
type FixSyntax = Fix (S.SyntaxF FixType FixBinder)

parsesTo :: String -> FixSyntax -> Expectation
parsesTo code expected =
  case snd (Parsing.parseString code) of
    Left e ->
      fail $ show e
    Right result -> do
      (result :: Parsing.AnnotatedSyntax)
      |> cata discardAnnotation
      |> (`shouldBe` expected)

discardAnnotation =
  Fix
  . S.mapCommon (CS.mapBinder discardBinderAnnotation)
  . S.mapType (cata Fix)

discardBinderAnnotation :: Parsing.SourceBinder -> FixBinder
discardBinderAnnotation =
  cata (Fix . S.mapBinderType (cata Fix))

typeAnnotation a b  = Fix $ S.typeAnnotation a b
symbol              = Fix . S.symbol
typeSymbol          = Fix . S.typeSymbol
application a b     = Fix $ S.application a b
function a b        = Fix $ S.function a b
binder              = Fix . S.binder
annotatedBinder a b = Fix $ S.annotatedBinder a b

