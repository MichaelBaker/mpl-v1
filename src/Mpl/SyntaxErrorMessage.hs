module Mpl.SyntaxErrorMessage where

import Mpl.ParserResult
import Mpl.Rendering
import Mpl.ParserDescription

errorMessage error =
  case errSpecific error of
    e@(SuggestionError {}) -> errorSuggestion e
    e@(ParserDescriptionError {}) -> errorParserDescription e
    a -> show error

synaxErrorHeader = header_ "Syntax Error"

errorParserDescription (ParserDescriptionError code description) = render message
  where top itemName =
                synaxErrorHeader
            <~> blankLine
            <~> "Found a syntactically incorrect " <~> callout itemName <~> "."
            <~> blankLine
            <~> "Here is what you wrote:"
            <~> blankLine
            <~> indent code
        message = case description of
          NameDescription itemName -> top itemName
          RichDescription itemName examples ->
                top itemName
            <~> blankLine
            <~> "Here are some valid examples:"
            <~> hardline
            <~> (indent $ stack $ map toDoc examples)

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
