module Mpl.Typed.Parsing where

import Mpl.Typed.Syntax (Syntax, int, symbol, application, typeAnnotation, typeSymbol)
import Mpl.Common.Parsing (Context(..), mkParser)
import Mpl.Common.ParsingUtils
  ( Result(Success)
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

typedContext = Context
  { mkInt         = int
  , mkSymbol      = symbol
  , mkApplication = application
  , mkExpression  = expression
  }

parser = mkParser typedContext

expression parseExpression alternative =
      parseTypeAnnotation parseExpression
  <|> alternative

parseTypeAnnotation parseExpression = do
  expression <- parseExpression
  annotation <- optional $ do
    symbolic ':'
    whiteSpace
    parseType
  return $ case annotation of
    Nothing  -> expression
    Just ann -> typeAnnotation expression ann

parseType = (typeSymbol <$> do
  firstChar <- oneOf upcaseChars          <?> "start of type symbol"
  rest      <- (many $ oneOf symbolChars) <?> "tail of type symbol"
  whiteSpace
  return $ stringToText (firstChar : rest)) <?> "type symbol"
