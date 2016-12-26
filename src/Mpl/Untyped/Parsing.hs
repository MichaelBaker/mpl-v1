module Mpl.Untyped.Parsing where

import Mpl.Untyped.Syntax
  ( SyntaxF
  , int
  , symbol
  , function
  , application
  )

import Mpl.Common.Parsers (commonParser)

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

import qualified Mpl.Common.Syntax as CS

parseExpressionText :: Text -> ParseResult (Parsed SyntaxF)
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consInt              = int
    , consSymbol           = symbol
    , consFunction         = function
    , consApplication      = application
    , consExpression       = id
    }
