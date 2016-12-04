module Mpl.Untyped.Parsing where

import Mpl.Untyped.Syntax (Syntax, int, symbol, application, function, leftAssociative, rightAssociative)
import Mpl.Common.Parsing (Context(..), mkParser)
import Mpl.Common.ParsingUtils
  ( Result
  , (<?>)
  , (<|>)
  , parseFromString
  , many
  , oneOf
  , whiteSpace
  , symbolic
  , optional
  , upcaseChars
  , symbolChars
  )

import Mpl.Utils
  ( Text
  , textToString
  , stringToText
  )

parseExpressionText :: Text -> Result Syntax
parseExpressionText = parseFromString parser . textToString

untypedContext = Context
  { mkInt              = int
  , mkSymbol           = symbol
  , mkFunction         = function
  , mkApplication      = application
  , mkExpression       = (<|>)
  , mkLeftAssociative  = leftAssociative
  , mkRightAssociative = rightAssociative
  }

parser = mkParser untypedContext
