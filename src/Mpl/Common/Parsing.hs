module Mpl.Common.Parsing where

import Data.Text         (Text)
import Mpl.Common.Syntax (Syntax, int, symbol, application)
import Mpl.Common.ParsingUtils
  ( Result()
  , (<?>)
  , (<|>)
  , parseFromString
  , textToString
  , stringToText
  , integer
  , many
  , some
  , oneOf
  , whiteSpace
  , try
  , parens
  )

parseExpressionText :: Text -> Result Syntax
parseExpressionText = parseFromString parser . textToString

parser = applicationOrExpression

applicationOrExpression =
      try parseApplication
  <|> parseExpression

parseExpression =
      parseInt
  <|> parseSymbol
  <|> parens applicationOrExpression

parseInt = int <$> integer

parseSymbol = (symbol <$> do
  firstChar <- oneOf symbolStartChars     <?> "start of symbol"
  rest      <- (many $ oneOf symbolChars) <?> "tail of symbol"
  whiteSpace
  return $ stringToText (firstChar : rest)) <?> "symbol"

parseApplication = application <$> parseSymbol <*> some parseExpression <?> "application"

symbolStartChars =
  ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars = symbolStartChars ++ digits

digits = "0123456789"
