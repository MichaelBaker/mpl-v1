module Mpl.Typed.ParsingSpec where

import Mpl.Typed.Parsing (parseExpressionText)
import Mpl.Typed.Syntax  (symbol, typeSymbol, typeAnnotation, application)
import TestUtils         (describe, it, shouldBe, mkParsesTo, mkIsSameAs)

parsesTo = mkParsesTo parseExpressionText
isSameAs = mkIsSameAs parseExpressionText

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
