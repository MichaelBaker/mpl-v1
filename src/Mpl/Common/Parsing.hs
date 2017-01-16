module Mpl.Common.Parsing where

import Mpl.Common.Syntax
  ( SyntaxF
  , Binder
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

parseExpressionText :: Text -> ParseResult (SourceAnnotated (SyntaxF (SourceAnnotated Binder)))
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consExpression       = id
    , consCommon           = id
    , consBinder           = id
    }
