module Mpl.Typed.ParsingSpec where

import Data.Functor.Foldable (Fix(..))
import Mpl.Typed.Parsing     (parseExpressionText)
import TestUtils             (describe, it, shouldBe, mkParsesTo, mkIsSameAs)

import qualified Mpl.Typed.Syntax as S

parsesTo = mkParsesTo parseExpressionText
isSameAs = mkIsSameAs parseExpressionText

typeAnnotation a b = Fix $ S.typeAnnotation a b
symbol             = Fix . S.symbol
typeSymbol         = S.typeSymbol
application a b    = Fix $ S.application a b

spec = do
  describe "Parsing" $ do
    it "parses type annotations" $ do
      "a: Integer" `parsesTo` (typeAnnotation (symbol "a") (typeSymbol "Integer"))

    it "parses type annotations for subexpressions" $ do
      "f (a: Integer)" `parsesTo`
        (application
          (symbol "f")
          [typeAnnotation (symbol "a") (typeSymbol "Integer")])

    it "binds type annotations to their closest expression" $ do
      "f a: Integer" `isSameAs` "f (a: Integer)"
