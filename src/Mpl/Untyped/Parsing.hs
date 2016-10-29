module Mpl.Untyped.Parsing where

import Mpl.Untyped.Syntax (Syntax, int, symbol, application)
import Mpl.Common.Parsing (Context(..), mkParser)
import Mpl.Common.ParsingUtils
  ( Text
  , Result
  , (<?>)
  , (<|>)
  , parseFromString
  , textToString
  , stringToText
  , many
  , oneOf
  , whiteSpace
  , symbolic
  , optional
  , upcaseChars
  , symbolChars
  )

parseExpressionText :: Text -> Result Syntax
parseExpressionText = parseFromString parser . textToString

untypedContext = Context
  { mkInt         = int
  , mkSymbol      = symbol
  , mkApplication = application
  , mkExpression  = (<|>)
  }

parser = mkParser untypedContext
