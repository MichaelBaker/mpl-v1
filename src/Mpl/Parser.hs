{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Control.Applicative ((<|>), many, some)
import Data.Char           (isSpace, isDigit, isAsciiUpper, isAsciiLower)
import Data.Text           (Text, pack)
import Text.Earley         ((<?>), Grammar, Report, Prod, list, satisfy, rule, fullParses, token, listLike)

import qualified Text.Earley as E

data AST = App   AST AST
         | Int   Text
         | Float Text
         | Ident Text
         deriving (Show, Eq)

parse :: Text -> ([AST], Report Text Text)
parse = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Char AST)
grammar = mdo
  skipWhitespace <- rule $ many $ satisfy isSpace

  app <- rule $ pure App
    <*  token '('
    <*  skipWhitespace
    <*> ident
    <*  skipWhitespace
    <*> (int <|> float)
    <*  skipWhitespace
    <*  token ')'
    <?> "application"

  ident <- rule $ (\firstLetter rest -> Ident $ pack $ firstLetter:rest)
    <$> satisfy isAsciiLower
    <*> many (satisfy $ \a -> any ($ a) [isAsciiLower, isAsciiUpper, isDigit])

  float <- rule $ (\firstDigit rest dot fraction -> Float $ pack $ firstDigit:rest ++ [dot] ++ fraction)
    <$> satisfy (`elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9'])
    <*> many (satisfy isDigit)
    <*> satisfy (== '.')
    <*> some (satisfy isDigit)
    <?> "float"

  int <- rule $ (\firstDigit rest -> Float $ pack $ firstDigit:rest)
    <$> satisfy (`elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9'])
    <*> many (satisfy isDigit)
    <?> "integer"

  return (int <|> app <|> float)
