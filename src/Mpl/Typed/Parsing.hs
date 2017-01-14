module Mpl.Typed.Parsing where

import Mpl.Typed.Syntax
  ( SyntaxF (Common)
  , int
  , binder
  , symbol
  , function
  , application
  , typeAnnotation
  , typeSymbol
  )

import Mpl.Common.Parsers (commonParser)

import Mpl.ParserUtils
  ( ParseResult
  , SyntaxConstructors(..)
  , SourceAnnotated
  , (<|>)
  , annotate
  , parseFromString
  , many
  , oneOf
  , whiteSpace
  , optional
  , upcaseChars
  , symbolChars
  , lookAhead
  , char
  )

import Mpl.Utils
  ( Text
  , textToString
  , stringToText
  )

import qualified Mpl.Common.Syntax as CS

parseExpressionText :: Text -> ParseResult (SourceAnnotated SyntaxF)
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consExpression       = parseTypeAnnotation
    , consCommon           = Common
    }

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
          annotation <- parseType
          return $ typeAnnotation expression annotation)

parseType =
  typeSymbol <$> do
    firstChar <- oneOf upcaseChars
    rest      <- (many $ oneOf symbolChars)
    whiteSpace
    return $ stringToText (firstChar : rest)
