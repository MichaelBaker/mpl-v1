module Mpl.Rendering.ParserError where

import           Data.Set
import           Mpl.ParserDescription
import           Mpl.ParserError
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Rendering
import           Mpl.Utils
import           Text.Trifecta.Delta
import qualified Data.ByteString.UTF8         as UTF8

errorMessage :: ByteString -> Error ParserState -> String
errorMessage byteString error = render $ (header_ "Syntax Error") <~> blankLine <~> message
  where stack = descriptionStack (parserState error)
        message =
          case stack of
            [] -> case errorReason error of
                    NoReason  ->
                      let summary =
                            if errorEOF error
                              then "The parser reached the end of the program unexpectedly."
                              else "The parser found an unexpected character, which is highlighed in red."
                      in    summary
                        <~> blankLine
                        <~> highlightedCode byteString [] (errorDelta error) (errorEOF error)
                    Message s ->
                          toDoc s
                      <~> blankLine
                      <~> highlightedCode byteString [] (errorDelta error) (errorEOF error)
            as@(a:_) ->
                  summary a
              <~> blankLine
              <~> (indent $ highlightedCode byteString as (errorDelta error) (errorEOF error))
              <~> blankLine
              <~> expectations (toAscList $ errorExpected error) (errorEOF error)
              <~> blankLine
              <~> examples (parserName a) (parserExamples a)

summary description = "The parser found a syntactically incorrect " <~> callout (parserName description) <~> "."

expectations [] _ =
  ""
expectations (a:[]) isEOF =
  "It expected to find " <~> suggestedAddition (parserExpectation a) <~> " " <~> expectationPreposition isEOF
expectations as isEOF =
      "It expected to find one of the following " <~> expectationPreposition isEOF
  <~> hardline
  <~> (indent $ stack $ fmap (toDoc . parserExpectation) as)

expectationPreposition True  = "at the position highlighted in red."
expectationPreposition False = "before the character highlighed in red."

examples _ [] =
  ""
examples itemName (a:[]) =
  "Here is an example " <~> toDoc itemName <~> ": " <~> toDoc a
examples itemName as =
      "Here are some example " <~> toDoc itemName <~> "s:"
  <~> blankLine
  <~> (indent $ stack $ fmap toDoc as)

highlightedCode byteString [] errorDelta isEOF =
      toDoc (upTo errorDelta byteString)
  <~> problemPart byteString errorDelta isEOF
  <~> toDoc (after errorDelta byteString)
highlightedCode byteString (a:[]) errorDelta isEOF =
      toDoc (upTo errorDelta byteString)
  <~> problemPart byteString errorDelta isEOF
  <~> toDoc (after errorDelta byteString)
highlightedCode byteString (a:_) errorDelta isEOF =
  let startDelta = parserDelta a
  in    toDoc   (upTo startDelta byteString)
    <~> callout (dropFromEnd 1 $ between startDelta errorDelta byteString)
    <~> callout (problemPart byteString errorDelta isEOF)
    <~> toDoc (after errorDelta byteString)

problemPart byteString errorDelta isEOF =
  if isEOF
    then toDoc (at errorDelta byteString) <~> problem_ "_"
    else problem (at errorDelta byteString)
