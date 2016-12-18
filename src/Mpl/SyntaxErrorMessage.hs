module Mpl.SyntaxErrorMessage where

import Mpl.ParserResult
import Mpl.Rendering

errorMessage error =
  case errSpecific error of
    e@(SuggestionError {}) -> errorSuggestion e
    e@(ParserDescriptionError {}) -> errorParserDescription e
    a -> show a

synaxErrorHeader = header_ "Syntax Error"

errorParserDescription (ParserDescriptionError description) =
  render
    $   synaxErrorHeader

errorSuggestion (SuggestionError itemName expectation example original) =
  render
    $   synaxErrorHeader
    <~> blankLine
    <~> "Found a syntactically incorrect " <~> callout itemName <~> "."
    <~> blankLine
    <~> expectation
    <~> blankLine
    <~> "Here is the " <~> itemName <~> " as you wrote it and an example that is syntactically correct:"
    <~> blankLine
    <~> "Original:"
    <~> hardline
    <~> indent original
    <~> blankLine
    <~> "Example:"
    <~> hardline
    <~> indent example
