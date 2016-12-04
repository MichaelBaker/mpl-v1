module Mpl.Common.Parsing where

import Mpl.Common.ParsingUtils
  ( Result
  , Parser
  , (<?>)
  , (<|>)
  , parseFromString
  , many
  , some
  , oneOf
  , whiteSpace
  , try
  , parens
  , symbolChars
  , symbolStartChars
  , symbolic
  , symbol
  , notFollowedBy
  , optional
  , digits
  , char
  )

import Mpl.Utils
  ( Text
  , textToString
  , stringToText
  )

data Context a = Context
  { mkInt              :: Integer -> a
  , mkSymbol           :: Text -> a
  , mkFunction         :: [a] -> a -> a
  , mkApplication      :: a -> [a] -> a
  , mkExpression       :: Parser a -> Parser a -> Parser a
  , mkLeftAssociative  :: a -> a
  , mkRightAssociative :: a -> a
  }

mkParser = parseApplicationOrExpression

parseExpression context = (mkExpression context) (parseAssociative context $ parseFlatExpression context) (parseAssociative context $ parens $ parseApplicationOrExpression context)
parseApplicationOrExpression context = (mkExpression context) (try $ parseApplication context) (parseExpression context)

parseAssociative context parser = do
      parseLeftAssociative context parser
  <|> parseMaybeRightAssociative context parser

parseLeftAssociative context parser= do
  symbolic '`' <?> "left associative backtick"
  notFollowedBy (symbolic '`')
  item <- parser
  notFollowedBy (symbolic '`')
  whiteSpace
  return (mkLeftAssociative context item)

parseMaybeRightAssociative context parser = do
  item <- parser
  backticks <- many (symbolic '`') <?> "right associative backtick"
  whiteSpace
  case length backticks of
    0 -> return item
    1 -> return (mkRightAssociative context item)
    _ -> fail ("A right associative expression has the form x`, not x" ++ backticks)

parseFlatExpression context =
      (parseInt context)
  <|> (parseFunction context)
  <|> (parseSymbol context)

parseInt context = do
  isMinus <- optional $ char '-'
  int     <- some (oneOf digits)
  case isMinus of
    Nothing -> return (mkInt context $ read int)
    Just _  -> return (mkInt context $ negate $ read int)

parseSymbol context = ((mkSymbol context) <$> do
  firstChar <- oneOf symbolStartChars     <?> "start of symbol"
  rest      <- (many $ oneOf symbolChars) <?> "tail of symbol"
  return $ stringToText (firstChar : rest)) <?> "symbol"

parseFunction context = (do
  symbol "#("
  parameters <- (some $ parseSymbol context <* whiteSpace) <?> "parameters"
  symbolic '='
  body <- (parseApplicationOrExpression context) <?> "body"
  symbolic ')'
  return (mkFunction context parameters body)
  ) <?> "function"

parseApplication context =
      (mkApplication context)
  <$> (parseExpression context)
  <*> some (parseExpression context)
  <?> "application"
