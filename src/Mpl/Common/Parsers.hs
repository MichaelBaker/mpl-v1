module Mpl.Common.Parsers where

import           Data.Function                ((&))
import           Data.List                    (intercalate)
import           Data.Text                    (dropWhileEnd)
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Rendering
import           Mpl.Utils
import qualified Mpl.Common.Syntax            as CS
import qualified Text.PrettyPrint.ANSI.Leijen as P

commonParser :: MplParser binder f
commonParser = parseApplicationOrExpression <* fileEnd

parseExpression :: MplParser binder f
parseExpression = makeExpression parseFlatExpression <|> (parens parseApplicationOrExpression)

parseApplicationOrExpression :: MplParser binder f
parseApplicationOrExpression = makeExpression (try parseApplication) <|> parseExpression

parseFlatExpression :: MplParser binder f
parseFlatExpression =
      parseInt
  <|> parseFunction
  <|> parseSymbol

parseInt :: MplParser binder f
parseInt =
  annotate
    "integer"
    "an integer"
    ["123", "0123", "-00000123"]
    (do
      isMinus <- optional $ char '-'
      int     <- some (oneOf digits)
      case isMinus of
        Nothing -> makeCommon $ CS.int $ read int
        Just _  -> makeCommon $ CS.int $ negate $ read int)

parseSymbol :: MplParser binder f
parseSymbol =
  annotate
    "symbol"
    "a symbol"
    ["a", "<?>", "Hello", "a0~"]
    (do
      text <- lift parseSymbolText
      makeCommon $ CS.Symbol text)

parseBinder :: StatefulParser (SourceAnnotated CS.Binder)
parseBinder =
  annotate'
    "binder"
    "a binder"
    ["a", "<?>", "Hello", "a0~"]
    (do
      text <- parseSymbolText
      return $ CS.Binder text)

parseSymbolText :: StatefulParser Text
parseSymbolText = do
  firstChar <- oneOf symbolStartChars
  rest      <- (many $ oneOf symbolChars)
  return $ stringToText (firstChar : rest)

parseFunction :: MplParser binder f
parseFunction =
  annotate
    "anonymous function"
    "an anonymous function"
    ["#(a = a + 1)", "#(a b f = f a b 3)"]
    (do
      char '#'
      char '('
      whiteSpace
      parameters <- sepEndBy1 (makeBinder parseBinder) someSpace
      char '='
      whiteSpace
      body <- withExpectation "expression" "an expression" parseApplicationOrExpression
      whiteSpace
      char ')'
      makeCommon $ CS.Function parameters body)

parseApplication :: MplParser binder f
parseApplication =
  annotate
    "function application"
    "a function application"
    ["f 1", "#(a = a + 1) 1"]
    (do
      function  <- parseExpression
      someSpace
      arguments <- sepEndBy1 parseExpression someSpace
      makeCommon $ CS.Application function arguments)
