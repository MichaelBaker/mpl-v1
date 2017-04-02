module Mpl.Typed.ParsingSpec where

import Data.Functor.Foldable (Fix(..))
import Mpl.Typed.Parsing     (parseExpressionText)
import Mpl.Utils             (cata)
import TestUtils             (describe, it, shouldBe, mkParsesTo, mkIsSameAs)

import qualified Mpl.Typed.Syntax  as S
import qualified Mpl.Common.Syntax as CS

parsesTo = mkParsesTo parseExpressionText (Fix . S.mapCommon (CS.mapBinder (cata Fix)))
isSameAs = mkIsSameAs parseExpressionText (Fix . S.mapCommon (CS.mapBinder (cata Fix)))

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
