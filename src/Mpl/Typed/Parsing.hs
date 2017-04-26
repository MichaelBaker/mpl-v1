module Mpl.Typed.Parsing where

import           Mpl.Common.Parsers
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Typed.Syntax
import           Mpl.Utils
import qualified Mpl.Common.Syntax  as CS

parseExpressionText :: Text -> ParseResult (SourceAnnotated (SyntaxF (SourceAnnotated Binder)))
parseExpressionText = parseFromString typedSyntaxConstructors commonParser . textToString

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
    Nothing  -> return expression
    Just _   ->
      annotate
        "type annotation"
        "a type annotation"
        ["a: Integer", "123 : Integer"]
        (do
          char ':'
          whiteSpace
          annotation <- lift parseType
          return $ typeAnnotation expression annotation)

parseType :: StatefulParser Type
parseType =
  typeSymbol <$> do
    firstChar <- oneOf upcaseChars
    rest      <- (many $ oneOf symbolChars)
    return $ stringToText (firstChar : rest)
