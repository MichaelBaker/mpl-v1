module Mpl.Common.Parsing where

import Mpl.Common.Syntax
  ( SyntaxF
  , int
  , symbol
  , function
  , application
  , leftAssociative
  , rightAssociative
  )

import Mpl.ParsingUtils
  ( Result
  , SyntaxConstructors(..)
  , Parsed
  , parseFromString
  )

import Mpl.Utils
  ( Text
  , textToString
  )

import Mpl.Common.Parsers (commonParser)

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
