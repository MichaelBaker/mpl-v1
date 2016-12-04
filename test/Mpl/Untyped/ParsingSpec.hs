module Mpl.Untyped.ParsingSpec where

import Mpl.Untyped.Parsing (parseExpressionText)
import Mpl.Untyped.Syntax  (int, symbol, application, function, leftAssociative, rightAssociative)
import TestUtils           (describe, it, shouldBe, mkParsesTo)

parsesTo = mkParsesTo parseExpressionText

spec = do
  describe "Parsing" $ do
    it "parses integers" $ do
      "1" `parsesTo` (int 1)

    it "parses symbols" $ do
      "f" `parsesTo` (symbol "f")

    it "parses symbols starting with #" $ do
      "#<>" `parsesTo` (symbol "#<>")

    it "parses prefix function application" $ do
      "f 1" `parsesTo`
        (application
          (symbol "f")
          [(int 1)])

      "f 1 2" `parsesTo`
        (application
          (symbol "f")
          [int 1, int 2])

      "f 1 2 3" `parsesTo`
        (application
          (symbol "f")
          [int 1, int 2, int 3])

    it "parses nested function application" $ do
      "f 1 (g 2 3) 4 (h 5)" `parsesTo`
        (application
          (symbol "f")
          [ int 1
          , application (symbol "g") [int 2, int 3]
          , int 4
          , application (symbol "h") [int 5]
          ])

    it "parses subexpression prefixes" $ do
      "(f 1 2) 1" `parsesTo`
        (application
          (application
            (symbol "f")
            [int 1, int 2])
          [(int 1)])

    it "parses function expressions" $ do
      "#(a = a)" `parsesTo`
        (function
          [symbol "a"]
          (symbol "a"))

    it "parses function expressions with multiple arguments" $ do
      "#(a b c = f 1 2 3)" `parsesTo`
        (function
          [symbol "a", symbol "b", symbol "c"]
          (application
            (symbol "f")
            [int 1, int 2, int 3]))

    it "parses application of a function" $ do
      "#(a = a) 1" `parsesTo`
        (application
          (function [symbol "a"] (symbol "a"))
          [int 1])

    it "parses prefix left associative functions" $ do
      "`+ 1" `parsesTo`
        (application
          (leftAssociative $ symbol "+")
          [int 1])

    it "parses prefix right associative functions" $ do
      "+` 1" `parsesTo`
        (application
          (rightAssociative $ symbol "+")
          [int 1])

    it "parses postfix left associative functions" $ do
      "1 `+" `parsesTo`
        (application
          (int 1)
          [leftAssociative $ symbol "+"])

    it "parses postfix right associative functions" $ do
      "1 +`" `parsesTo`
        (application
          (int 1)
          [rightAssociative $ symbol "+"])

    it "parses infix right associative functions" $ do
      "1 +` 2 +` 3" `parsesTo`
        (application
          (int 1)
          [ rightAssociative (symbol "+")
          , int 2
          , rightAssociative (symbol "+")
          , int 3
          ])

    it "parses infix left associative functions" $ do
      "1 `+ 2 `+ 3" `parsesTo`
        (application
          (int 1)
          [ leftAssociative (symbol "+")
          , int 2
          , leftAssociative (symbol "+")
          , int 3
          ])

