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
  , Parsed
  , parseFromString
  )

import Mpl.Utils
  ( Text
  , textToString
  )

import Mpl.Common.Parsers (commonParser)

parseExpressionText :: Text -> ParseResult (Parsed SyntaxF)
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consInt              = int
    , consBinder           = binder
    , consSymbol           = symbol
    , consFunction         = function
    , consApplication      = application
    , consExpression       = id
    }
