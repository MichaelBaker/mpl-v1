module Mpl.Common.Parsing where

import Mpl.Common.Syntax
  ( SyntaxF
  , int
  , binder
  , symbol
  , function
  , application
  )

import Mpl.ParserUtils
  ( ParseResult
  , SyntaxConstructors(..)
  , SourceAnnotated
  , parseFromString
  )

import Mpl.Utils
  ( Text
  , textToString
  )

import Mpl.Common.Parsers (commonParser)

parseExpressionText :: Text -> ParseResult (SourceAnnotated SyntaxF)
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consExpression       = id
    , consCommon           = id
    }
