module Mpl.Rendering.TypeError where

import           Mpl.Prelude
import           Mpl.ParserUtils
import           Mpl.Rendering
import           Mpl.Typed.Typecheck
import           Data.Text.Metrics
import           Data.Map.Strict     as Map
import           Data.List           as List

errorMessage :: ByteString -> Context -> TypeError -> String
errorMessage byteString context error =
  render
    $   (header_ "Type Error")
    <~> blankLine
    <~> message byteString context error

message byteString context (UnboundTypeSymbol span symbol) =
  unboundTypeSymbol byteString context span symbol

message byteString context (ApplicationOfNonFunction span type_) =
  applicationOfNonFunction byteString context span type_

message _ _ e = toDoc $ show e

unboundTypeSymbol byteString context span symbol =
      "The type " <~> callout symbol <~> " isn't defined in the following type annotation."
  <~> blankLine
  <~> indent
    (   toDoc (upTo (startDelta span) byteString)
    <~> problem (between (startDelta span) (endDelta span) byteString)
    <~> toDoc (after (endDelta span) byteString)
    )
  <~> suggestions
  where possibleTypes =
          Map.keys (typeSymbolTypes context) |> List.filter ((< 3) . levenshtein symbol)

        suggestions =
          if not (List.null possibleTypes)
            then
                  blankLine
              <~> "Here are some defined types that are similar to " <~> toDoc symbol <~> ":"
              <~> blankLine
              <~> indent (stack $ fmap toDoc possibleTypes)
            else
              ""

applicationOfNonFunction byteString context span type_ =
      "The following value is being used as a function, but it has type " <~> callout type_ <~> "."
  <~> blankLine
  <~> indent
    (   toDoc (upTo (startDelta span) byteString)
    <~> problem (between (startDelta span) (endDelta span) byteString)
    <~> toDoc (after (endDelta span) byteString)
    )
