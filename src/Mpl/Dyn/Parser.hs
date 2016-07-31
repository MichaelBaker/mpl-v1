{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Mpl.Dyn.AST               (AST(..), Span(..))
import Control.Applicative       ((<|>), many)
import Data.Text                 (pack)
import Data.ByteString.Char8     (unpack)
import Data.Char                 (isAsciiUpper, isAsciiLower, isDigit)
import Text.Parser.Char          (satisfy)
import Text.Parser.Token         (TokenParsing(), integer, whiteSpace, double, parens, brackets)
import Text.Parser.Combinators   (try, optional, sepBy)
import Text.Trifecta.Delta       (Delta(Directed))
import Text.Trifecta.Result      (Result())
import Text.Trifecta.Parser      (parseFromFileEx)
import Text.Trifecta.Combinators (DeltaParsing(), position)

data ParseType = Exp | Prog

parseFile :: ParseType -> String -> IO (Result AST)
parseFile Exp  filepath = parseFromFileEx expression filepath
parseFile Prog filepath = parseFromFileEx program filepath

program = withSpan $ AProg <$> many (definition <* whiteSpace)

expression = lambda <|> try real <|> int

char = satisfy . (==)

withSpan :: (DeltaParsing m) => m (Span -> a) -> m a
withSpan parser = do
  startSpan <- position
  item      <- parser
  endSpan   <- position
  let (filePath, startByte) = case startSpan of
                                Directed file _ _ bytes _ -> (file, bytes)
                                _ -> error $ "Invalid start delta: " ++ show startSpan
  let endByte = case endSpan of
                  Directed _ _ _ bytes _ -> bytes
                  _ -> error $ "Invalid end delta: " ++ show endSpan
  return $ item (Span (unpack filePath) startByte endByte)

lambda = withSpan $ parens $ do
  char '#'
  whiteSpace
  args <- try arguments <|> return []
  whiteSpace
  body <- expression
  return $ ALam args body

arguments = brackets (sepBy symbol whiteSpace)

int = withSpan $ AInt <$> integer

real = withSpan $ do
  neg <- optional (satisfy (== '-'))
  val <- double
  return $ case neg of
    Nothing -> AReal val
    Just _  -> AReal (-val)

definition = withSpan $ do
  sym <- symbol
  whiteSpace
  satisfy (== '=')
  whiteSpace
  val <- expression
  return $ ADef sym val

symbol :: (Monad m, TokenParsing m, DeltaParsing m) => m AST
symbol = withSpan $ do
  firstChar <- satisfy (\a -> any ($ a) [isAsciiUpper, isAsciiLower, (`elem` symChars)])
  rest      <- many $ satisfy (\a -> any ($ a) [isAsciiUpper, isAsciiLower, (`elem` symChars), isDigit])
  return $ ASym $ pack (firstChar : rest)

symChars = ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '\\', '/', '.']
