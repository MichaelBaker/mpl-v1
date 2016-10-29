module Mpl.Untyped.ParsingSpec where

import Mpl.Untyped.Parsing (parseExpressionText)
import Mpl.Untyped.Syntax  (int, symbol, application)
import TestUtils           (describe, it, shouldBe, mkParsesTo)

parsesTo = mkParsesTo parseExpressionText

spec = do
  describe "Parsing" $ do
    it "parses integers" $ do
      "1" `parsesTo` (int 1)

    it "parses symbols" $ do
      "f" `parsesTo` (symbol "f")

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
