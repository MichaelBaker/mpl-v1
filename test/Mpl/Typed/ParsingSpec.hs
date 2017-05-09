module Mpl.Typed.ParsingSpec where

import           Mpl.Prelude
import           Mpl.Typed.Parsing
import           Mpl.Utils
import           TestUtils
import qualified Mpl.Common.Syntax as CS
import qualified Mpl.Typed.Syntax  as S

isSameAs a b =
  case snd $ parseExpressionText a of
    Left e -> fail $ show e
    Right resultA ->
      case snd $ parseExpressionText b of
        Left e ->
          fail $ show e
        Right resultB -> do
          (cata discardAnnotation resultA) `shouldBe` (cata discardAnnotation resultB)

parsesTo :: Text -> Fix (S.SyntaxF (Fix S.Binder)) -> Expectation
parsesTo text expected =
  case snd $ parseExpressionText text of
    Left e ->
      fail $ show e
    Right result -> do
      result
      |> cata discardAnnotation
      |> (`shouldBe` expected)

discardAnnotation =
  Fix . S.mapCommon (CS.mapBinder (cata Fix))

typeAnnotation a b  = Fix $ S.typeAnnotation a b
symbol              = Fix . S.symbol
typeSymbol          = S.typeSymbol
application a b     = Fix $ S.application a b
function a b        = Fix $ S.function a b
binder              = Fix . S.binder
annotatedBinder a b = Fix $ S.annotatedBinder a b

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
