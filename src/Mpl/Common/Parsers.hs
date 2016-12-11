module Mpl.Common.Parsers where

import Mpl.ParsingUtils
  ( Result
  , Parser
  , Parsed
  , MplParser
  , MplAnnotatable
  , Parsable
  , ParserDescription(..)
  , ParserSuggestion(..)
  , Err(..)
  , (!>)
  , makeInt
  , makeSymbol
  , makeFunction
  , makeApplication
  , makeExpression
  , makeLeftAssociative
  , makeRightAssociative
  , annotate
  , (<|>)
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
  , lookAhead
  )

import Mpl.Utils
  ( Text
  , textToString
  , stringToText
  , byteStringToString
  )

import Data.List (intercalate)

commonParser :: (Parsable f) => MplParser f
commonParser = parseApplicationOrExpression

parseExpression :: (Parsable f) => MplParser f
parseExpression = makeExpression (parseAssociative parseFlatExpression) <|> (parseAssociative $ parens parseApplicationOrExpression)

parseApplicationOrExpression :: (Parsable f) => MplParser f
parseApplicationOrExpression = makeExpression (try parseApplication) <|> parseExpression

parseAssociative :: (Parsable f) => MplParser f -> MplParser f
parseAssociative parser = do
      parseLeftAssociative parser
  <|> parseMaybeRightAssociative parser

parseLeftAssociative :: (Parsable f) => MplParser f -> MplParser f
parseLeftAssociative parser =
  annotate
    "left associative expression"
    ["`x", "`#(a = a + 1)"]
    (do
      symbolic '`'
      notFollowedBy (symbolic '`')
      item <- parser
      notFollowedBy (symbolic '`')
      whiteSpace
      makeLeftAssociative item)

parseMaybeRightAssociative :: (Parsable f) => MplParser f -> MplParser f
parseMaybeRightAssociative parser = do
  item <- parser
  hasBacktick <- lookAhead (optional $ char '`')
  case hasBacktick of
    Nothing -> whiteSpace >> return item
    Just _  ->
      annotate
        "right associative expression"
        ["x`", "#(a = a + 1)`"]
        (do
          backticks <- many (symbolic '`')
          whiteSpace
          if length backticks > 1
            then fail ("A right associative expression has the form x`, not x" ++ backticks)
            else makeRightAssociative item)

parseFlatExpression :: (Parsable f) => MplParser f
parseFlatExpression =
      parseInt
  <|> parseFunction
  <|> parseSymbol

parseInt :: (Parsable f) => MplParser f
parseInt =
  annotate
    "integer"
    ["123", "0123", "-00000123"]
    (do
      isMinus <- optional $ char '-'
      int     <- some (oneOf digits)
      case isMinus of
        Nothing -> makeInt $ read int
        Just _  -> makeInt $ negate $ read int)

parseSymbol :: (Parsable f) => MplParser f
parseSymbol =
  annotate
    "symbol"
    ["a", "<?>", "Hello", "a0~"]
    (do
      firstChar <- oneOf symbolStartChars
      rest      <- (many $ oneOf symbolChars)
      makeSymbol $ stringToText (firstChar : rest))

parseFunction :: (Parsable f) => MplParser f
parseFunction =
  annotate
    "anonymous function"
    ["#(a = a + 1)", "#(a b f = f a b 3)"]
    (do
      symbol "#("
      parameters <- (some $ parseSymbol <* whiteSpace)
      symbolic '=' !> (missingEqual parameters)
      body <- parseApplicationOrExpression
      symbolic ')'
      makeFunction parameters body)
  where missingEqual :: (Show a) => [a] -> ParserDescription -> Err -> Err
        missingEqual params parentDescription error =
          let paramList     = intercalate " " $ map show params
              body          = show $ head params
              customExample = concat ["#(", paramList, " = ", body, ")"]
              suggestion    =
                ParserSuggestion
                { itemName    = name parentDescription
                , expectation = "The parser expected to find an equal sign, but the function just ended."
                , example     = customExample
                }
          in error { _errSuggestion = Just suggestion }

parseApplication :: (Parsable f) => MplParser f
parseApplication =
  annotate
    "function application"
    ["f 1", "#(a = a + 1) 1"]
    (do
      function  <- parseExpression
      arguments <- some parseExpression
      makeApplication function arguments)
