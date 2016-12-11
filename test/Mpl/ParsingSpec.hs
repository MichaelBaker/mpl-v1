module Mpl.ParsingSpec where

import Data.Functor.Foldable (Fix(..))
import Mpl.Common.Parsing    (parseExpressionText)
import TestUtils             (describe, it, shouldBe, mkParserErrorsWith)

import qualified Mpl.Common.Syntax as S

errorsWith = mkParserErrorsWith parseExpressionText

int              = Fix . S.int
symbol           = Fix . S.symbol
application a b  = Fix $ S.application a b
function a b     = Fix $ S.function a b
leftAssociative  = Fix . S.leftAssociative
rightAssociative = Fix . S.rightAssociative

spec = do
  describe "Parsing Errors" $ do
    it "gives an example for a function missing an equal sign" $ do
      "#(a)" `errorsWith` unlines
        [ "Found a syntactically incorrect anonymous function."
        , ""
        , "The parser expected to find an equal sign, but the function just ended."
        , ""
        , "Here is the anonymous function as you wrote it and an example that is syntactically correct:"
        , ""
        , "Original:"
        , "  #(a)"
        , ""
        , "Example:"
        , "  #(a = a)"
        ]
