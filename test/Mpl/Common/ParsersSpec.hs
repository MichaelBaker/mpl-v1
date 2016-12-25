module Mpl.Common.ParsersSpec where

import Data.Functor.Foldable  (Fix(..))
import Data.List              (intercalate)
import Mpl.Common.Parsing     (parseExpressionText)
import Mpl.ParserResult       (SpecificError(..))
import Mpl.Rendering
import Mpl.SyntaxErrorMessage (errorSuggestion)
import TestUtils              (describe, it, shouldBe, mkParserErrorsWith)

import qualified Mpl.Common.Syntax as S

errorsWith = mkParserErrorsWith parseExpressionText

spec = do
  describe "Parsing Errors" $ do
    it "gives an example for a function missing an equal sign" $ do
      "#(a)" `errorsWith`
        (errorSuggestion $
          SuggestionError
            "anonymous function"
            "The parser expected to find an equal sign, but the function just ended."
            ("#(a" <~> suggestedAddition_ " = a" <~> ")")
            ("#(a" <~> problem_ ")"))

    it "gives an example for a function missing an body" $ do
      "#(a =)" `errorsWith`
        (errorSuggestion $
          SuggestionError
          "anonymous function"
          "The parser expected to find an expression in the body, but the function just ended."
          ("#(a = " <~> suggestedAddition_ "a" <~> ")")
          ("#(a =" <~> problem_ ")"))

    it "gives an example for a function missing an closing parenthesis" $ do
      "#(a = a" `errorsWith`
        (errorSuggestion $
          SuggestionError
          "anonymous function"
          "The parser expected to find a closing parenthesis after the function body, but there wasn't one."
          ("#(a = a" <~> suggestedAddition_ ")")
          ("#(a = a" <~> problem_ ""))

    it "explains the rules of symbols" $ do
      "0a" `errorsWith` "whoops"
