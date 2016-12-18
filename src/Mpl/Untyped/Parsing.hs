module Mpl.Untyped.Parsing where

import Mpl.Untyped.Syntax
  ( SyntaxF
  , int
  , symbol
  , function
  , application
  , leftAssociative
  , rightAssociative
  )

import Mpl.Common.Parsers (commonParser)

import Mpl.ParserUtils
  ( Result
  , SyntaxConstructors(..)
  , Parsed
  , parseFromString
  )

import Mpl.Utils
  ( Text
  , textToString
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
    , consExpression       = id
    , consLeftAssociative  = leftAssociative
    , consRightAssociative = rightAssociative
    }
