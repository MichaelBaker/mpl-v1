module Mpl.Common.ParsingSpec where

import Data.Functor.Foldable (Fix(..))
import Mpl.Common.Parsing    (parseExpressionText)
import TestUtils             (describe, it, shouldBe, mkParsesTo)

import qualified Mpl.Common.Syntax as S

parsesTo = mkParsesTo parseExpressionText

int              = Fix . S.int
binder           = Fix . S.binder
symbol           = Fix . S.symbol
application a b  = Fix $ S.application a b
function a b     = Fix $ S.function a b

spec = do
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

  it "parses function expressions" $ do
    "#(a = a)" `parsesTo`
      (function
        [binder "a"]
        (symbol "a"))

  it "parses function expressions with multiple arguments" $ do
    "#(a b c = f 1 2 3)" `parsesTo`
      (function
        [binder "a", binder "b", binder "c"]
        (application
          (symbol "f")
          [int 1, int 2, int 3]))

  it "parses application of a function" $ do
    "#(a = a) 1" `parsesTo`
      (application
        (function [binder "a"] (symbol "a"))
        [int 1])
