module Mpl.Common.ParsersSpec where

import           Mpl.Common.Parsing
import           Mpl.ParserError
import           Mpl.ParserUtils
import           Mpl.Rendering.ParserError
import           Mpl.Rendering
import           TestUtils
import qualified Mpl.Common.Syntax         as S
import qualified Data.List                 as List

spec = do
  describe "syntax errors" $ do
    it "handles functions missing an equal sign" $ do
      "#(a b)" `errorContains` (callout_ "anonymous function")

    it "handles functions missing a closing parenthesis" $ do
      "#(a b =" `errorContains` (callout_ "anonymous function")

    it "handles malformed integers" $ do
      "789a" `errorContains` (text "unexpected character")

    it "handles nested functions" $ do
      "#(c = #(a b =)" `errorContains` (callout_ "anonymous function")

    it "handles unmatched quotes" $ do
      "\"abc" `errorContains` (callout_ "UTF8 string")

errorContains code expected =
  case parseExpressionText code of
    (bs, Left e) ->
      if List.isInfixOf (render expected) (errorMessage bs e)
        then return ()
        else do
          expectationFailure $ concat
            [ "\n"
            , "==== Expected " ++ show code ++ " to contain the string:\n\n"
            , render expected
            , "\n\n"
            , "==== This was the error that was produced:\n\n"
            , errorMessage bs e
            , "\n"
            ]
    (_, Right a) -> fail $ "Successfully parsed " ++ show code

