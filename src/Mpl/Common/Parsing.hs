module Mpl.Common.Parsing where

import Mpl.Common.ParsingUtils
  ( Text
  , Result
  , Parser
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
  , symbolChars
  , symbolStartChars
  )

data Context a = Context
  { mkInt           :: Integer -> a
  , mkSymbol        :: Text -> a
  , mkApplication   :: a -> [a] -> a
  , mkExpression    :: Parser a -> Parser a -> Parser a
  }

mkParser = parseApplicationOrExpression

parseExpression context = (mkExpression context) (parseFlatExpression context) (parens $ parseApplicationOrExpression context)
parseApplicationOrExpression context = (mkExpression context) (try $ parseApplication context) (parseExpression context)

parseFlatExpression context =
      (parseInt context)
  <|> (parseSymbol context)

parseInt context = (mkInt context) <$> integer

parseSymbol context = ((mkSymbol context) <$> do
  firstChar <- oneOf symbolStartChars     <?> "start of symbol"
  rest      <- (many $ oneOf symbolChars) <?> "tail of symbol"
  whiteSpace
  return $ stringToText (firstChar : rest)) <?> "symbol"

parseApplication context =
      (mkApplication context)
  <$> (parseExpression context)
  <*> some (parseExpression context)
  <?> "application"
