{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Mpl.Dyn.AST             (AST(..))
import Control.Applicative     ((<|>), many)
import Data.Text               (pack)
import Data.Char               (isAsciiUpper, isAsciiLower, isDigit)
import Text.Parser.Char        (satisfy)
import Text.Parser.Token       (TokenParsing(), integer, whiteSpace, double)
import Text.Parser.Combinators (try, optional)
import Text.Trifecta.Result    (Result())
import Text.Trifecta.Parser    (parseFromFileEx)

data ParseType = Exp | Prog

parseFile :: ParseType -> String -> IO (Result AST)
parseFile Exp  filepath = parseFromFileEx expression filepath
parseFile Prog filepath = parseFromFileEx program filepath

program = AProg <$> many (definition <* whiteSpace)

expression = try real <|> int

int = AInt <$> integer

real = do
  neg <- optional (satisfy (== '-'))
  val <- double
  return $ case neg of
    Nothing -> AReal val
    Just _  -> AReal (-val)

definition = do
  sym <- symbol
  whiteSpace
  satisfy (== '=')
  whiteSpace
  val <- expression
  return $ ADef sym val

symbol :: (Monad m, TokenParsing m) => m AST
symbol = do
  firstChar <- satisfy (\a -> any ($ a) [isAsciiUpper, isAsciiLower, (`elem` symChars)])
  rest      <- many $ satisfy (\a -> any ($ a) [isAsciiUpper, isAsciiLower, (`elem` symChars), isDigit])
  return $ ASym $ pack (firstChar : rest)

symChars = ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '\\', '/', '.']
