module Mpl.Typed.Parsing where

import Mpl.Typed.Syntax
  ( SyntaxF
  , int
  , symbol
  , function
  , application
  , leftAssociative
  , rightAssociative
  , typeAnnotation
  , typeSymbol
  )

import Mpl.Common.Parsers (commonParser)

import Mpl.ParserUtils
  ( Result(Success)
  , SyntaxConstructors(..)
  , Parsed
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

parseExpressionText :: Text -> Result (Parsed SyntaxF)
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consInt              = int
    , consSymbol           = symbol
    , consFunction         = function
    , consApplication      = application
    , consExpression       = parseTypeAnnotation
    , consLeftAssociative  = leftAssociative
    , consRightAssociative = rightAssociative
    }

parseTypeAnnotation parseExpression = do
  expression <- parseExpression
  annotation <- lookAhead (optional $ char ':')
  case annotation of
    Nothing  -> return expression
    Just _   ->
      annotate
        "Type annotation"
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
