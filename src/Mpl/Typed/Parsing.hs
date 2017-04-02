module Mpl.Typed.Parsing where

import Mpl.Typed.Syntax
  ( SyntaxF (Common)
  , Binder  (CommonBinder)
  , Type
  , int
  , binder
  , symbol
  , function
  , application
  , typeAnnotation
  , typeSymbol
  , annotatedBinder
  )

import Mpl.Common.Parsers (commonParser)

import Mpl.ParserUtils
  ( ParseResult
  , SyntaxConstructors(..)
  , SourceAnnotated
  , StatefulParser
  , (<|>)
  , annotate
  , annotate'
  , parseFromString
  , many
  , oneOf
  , whiteSpace
  , optional
  , upcaseChars
  , symbolChars
  , lookAhead
  , char
  , lift
  )

import Mpl.Utils
  ( Text
  , textToString
  , stringToText
  , mapAnnotated
  )

import qualified Mpl.Common.Syntax as CS

parseExpressionText :: Text -> ParseResult (SourceAnnotated (SyntaxF (SourceAnnotated Binder)))
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consExpression       = parseTypeAnnotation
    , consCommon           = Common
    , consBinder           = parseBinder
    }

parseBinder parseCommonBinder = do
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
