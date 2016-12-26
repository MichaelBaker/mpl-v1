module Mpl.Common.Parsers where

import Mpl.ParserUtils
  ( Parser
  , Parsed
  , MplParser
  , MplAnnotatable
  , Parsable
  , makeInt
  , makeSymbol
  , makeFunction
  , makeApplication
  , makeExpression
  , annotate
  , (<|>)
  , many
  , some
  , oneOf
  , noneOf
  , whiteSpace
  , someSpace
  , sepEndBy1
  , try
  , parens
  , symbolChars
  , symbolStartChars
  , symbol
  , notFollowedBy
  , optional
  , digits
  , char
  , lookAhead
  , fileEnd
  , withExpectation
  )

import Data.Function ((&))
import Data.Text     (dropWhileEnd)

import qualified Text.PrettyPrint.ANSI.Leijen as P

import Mpl.Rendering

import Mpl.Utils
  ( Text
  , textToString
  , stringToText
  , byteStringToString
  )

import Data.List (intercalate)

commonParser :: (Parsable f) => MplParser f
commonParser = parseApplicationOrExpression <* fileEnd

parseExpression :: (Parsable f) => MplParser f
parseExpression = makeExpression parseFlatExpression <|> (parens parseApplicationOrExpression)

parseApplicationOrExpression :: (Parsable f) => MplParser f
parseApplicationOrExpression = makeExpression (try parseApplication) <|> parseExpression

parseFlatExpression :: (Parsable f) => MplParser f
parseFlatExpression =
      parseInt
  <|> parseFunction
  <|> parseSymbol

parseInt :: (Parsable f) => MplParser f
parseInt =
  annotate
    "integer"
    "an integer"
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
    "a symbol"
    ["a", "<?>", "Hello", "a0~"]
    (do
      firstChar <- oneOf symbolStartChars
      rest      <- (many $ oneOf symbolChars)
      makeSymbol $ stringToText (firstChar : rest))

parseFunction :: (Parsable f) => MplParser f
parseFunction =
  annotate
    "anonymous function"
    "an anonymous function"
    ["#(a = a + 1)", "#(a b f = f a b 3)"]
    (do
      char '#'
      char '('
      whiteSpace
      parameters <- sepEndBy1 parseSymbol someSpace
      char '='
      whiteSpace
      body <- withExpectation "expression" "an expression" parseApplicationOrExpression
      whiteSpace
      char ')'
      makeFunction parameters body)

parseApplication :: (Parsable f) => MplParser f
parseApplication =
  annotate
    "function application"
    "a function application"
    ["f 1", "#(a = a + 1) 1"]
    (do
      function  <- parseExpression
      someSpace
      arguments <- sepEndBy1 parseExpression someSpace
      makeApplication function arguments)
