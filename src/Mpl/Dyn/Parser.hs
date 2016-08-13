{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Mpl.Dyn.AST               (AST(..), Span(..))
import Control.Applicative       ((<|>), many)
import Data.Text                 (pack)
import Data.ByteString.Char8     (unpack)
-- import Data.Char                 (isAsciiUpper, isAsciiLower, isDigit)
import Text.Parser.Char          (oneOf)
import Text.Parser.Token         (TokenParsing(), integer, whiteSpace, double, parens, brackets, braces, symbolic)
import Text.Parser.Combinators   ((<?>), try, optional, sepBy, manyTill)
import Text.Trifecta.Delta       (Delta(Directed))
import Text.Trifecta.Result      (Result())
import Text.Trifecta.Parser      (parseFromFileEx)
import Text.Trifecta.Combinators (DeltaParsing(), position)

import qualified Text.Trifecta.Delta  as Delta
import qualified Text.Trifecta.Parser as Parser

data ParseType = Exp | Prog | Def

parseFile :: ParseType -> String -> IO (Result AST)
parseFile Exp  filepath = parseFromFileEx expression filepath
parseFile Prog filepath = parseFromFileEx program filepath
parseFile Def  filepath = parseFromFileEx definition filepath

parseString :: ParseType -> String -> Result AST
parseString Exp  string = Parser.parseString expression zeroDelta string
parseString Prog string = Parser.parseString program    zeroDelta string
parseString Def  string = Parser.parseString definition zeroDelta string

program = withSpan $ AProg <$> recursiveDefinitions

expression = lambda <|> try real <|> int <|> record <|> symbol

recursiveDefinitions = withSpan $ ARecDefs <$> many (definition <* whiteSpace)

record = withSpan $ braces $ ARec <$> optionalTrailing recordField (symbolic ',')

recordField = withSpan $ AField <$> fieldLabel <* whiteSpace <* symbolic ':' <* whiteSpace <*> expression

fieldLabel = int <|> symbol

lambda = withSpan $ parens $ do
  symbolic '#'
  whiteSpace
  args <- try (brackets $ sepBy symbol whiteSpace) <|> return []
  whiteSpace
  body <- expression
  return $ ALam args body

int = withSpan $ AInt <$> integer

real = withSpan $ do
  neg <- optional (symbolic '-')
  val <- double
  return $ case neg of
    Nothing -> AReal val
    Just _  -> AReal (-val)

definition = withSpan $ do
  startSpan <- position
  sym <- symbol
  whiteSpace
  args <- manyTill (symbol <* whiteSpace) (symbolic '=')
  whiteSpace
  body <- expression
  endSpan <- position
  return $ if null args
    then ADef sym body
    else ADef sym (ALam args body $ makeSpan startSpan endSpan)

symbol :: (Monad m, TokenParsing m, DeltaParsing m) => m AST
symbol = withSpan $ do
  firstChar <- oneOf symStartChars <?> "start of symbol"
  rest      <- (many $ oneOf symChars) <?> "tail of symbol"
  return $ ASym $ pack (firstChar : rest)

reservedChars = ['.']

symStartChars =
  ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '\\', '/'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

symChars = symStartChars ++ digits

digits = "0123456789"

optionalTrailing a sep = do
  values   <- many $ try (a <* whiteSpace <* sep <* whiteSpace)
  trailing <- optional a
  return $ case trailing of
    Nothing -> values
    Just a  -> values ++ [a]

withSpan :: (DeltaParsing m) => m (Span -> a) -> m a
withSpan parser = do
  startSpan <- position
  item      <- parser
  endSpan   <- position
  return $ item (makeSpan startSpan endSpan)

makeSpan startSpan endSpan = Span (unpack filePath) startByte endByte
  where (filePath, startByte) = case startSpan of
                                  Directed file _ _ bytes _ -> (file, bytes)
                                  _ -> error $ "Invalid start delta: " ++ show startSpan
        endByte = case endSpan of
                    Directed _ _ _ bytes _ -> bytes
                    _ -> error $ "Invalid end delta: " ++ show endSpan

zeroDelta = Delta.Columns 0 0
