module Mpl.Typed.Parsing where

import           Mpl.Common.Parsers
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Typed.Syntax
import           Mpl.Utils
import qualified Mpl.Common.Syntax  as CS

type SourceType   = SourceAnnotated Type
type SourceBinder = SourceAnnotated (Binder SourceType)
type SourceSyntax = SourceAnnotated (SyntaxF SourceType SourceBinder)

parseExpressionText :: Text -> ParseResult SourceSyntax
parseExpressionText =
  parseFromString typedSyntaxConstructors commonParser . textToString

typedSyntaxConstructors =
  SyntaxConstructors
    { consExpression       = parseTypeAnnotation
    , consCommon           = Common
    , consBinder           = typedParseBinder
    }

typedParseBinder parseCommonBinder = do
  binder     <- fmap (mapAnnotated CommonBinder) parseCommonBinder
  annotation <- lookAhead (optional $ char ':')
  case annotation of
    Nothing  ->
      return binder
    Just _   ->
      annotate'
        "type annotation"
        "a type annotation"
        ["a: Integer", "123 : Integer"]
        (do
          char ':'
          whiteSpace
          annotation <- parseType
          return $ annotatedBinder binder annotation)

parseTypeAnnotation parseExpression = do
  expression <- parseExpression
  annotation <- lookAhead (optional $ char ':')
  case annotation of
    Nothing ->
      return expression
    Just _ ->
      annotate
        "type annotation"
        "a type annotation"
        ["a: Integer", "123 : Integer"]
        (do
          char ':'
          whiteSpace
          annotation <- lift parseType
          return $ typeAnnotation expression annotation)

parseType =
  annotate'
    "type"
    "a type"
    ["Integer"]
    (do
      firstChar <- oneOf upcaseChars
      rest      <- (many $ oneOf symbolChars)
      let symbol = stringToText (firstChar : rest)
      return $ typeSymbol symbol)
