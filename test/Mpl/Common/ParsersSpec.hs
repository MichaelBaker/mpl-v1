module Mpl.Common.ParsersSpec where

import Data.Functor.Foldable  (Fix(..))
import Mpl.Common.Parsing     (parseExpressionText)
import TestUtils              (describe, it, shouldBe, mkParserErrorContains)
import Mpl.Rendering.ParserError
import Mpl.ParserError
import Mpl.ParserUtils

import qualified Mpl.Common.Syntax as S

errorContains = mkParserErrorContains parseExpressionText

spec = do
  describe "syntax errors" $ do
    it "handles functions missing an equal sign" $ do
      "#(a b)" `errorContains` "anonymous function"

    it "handles functions missing a closing parenthesis" $ do
      "#(a b =" `errorContains` "anonymous function"

    it "handles malformed integers" $ do
      "789a" `errorContains` "unexpected character"

    it "handles nested functions" $ do
      "#(c = #(a b =)" `errorContains` "anonymous function"
