{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Control.Applicative ((<|>), many)
import Data.Char           (isSpace, isDigit)
import Data.Text           (Text, pack)
import Text.Earley         ((<?>), Grammar, Report, Prod, list, satisfy, rule, fullParses, token, listLike)

import qualified Text.Earley as E

data AST = App Text AST
         | Int Text
         deriving (Show, Eq)

parse :: Text -> ([AST], Report Text Text)
parse = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Char AST)
grammar = mdo
  skipWhitespace <- rule $ many $ satisfy isSpace

  app <- rule $ pure App
    <*  token '('
    <*  skipWhitespace
    <*> listLike "f"
    <*  skipWhitespace
    <*> int
    <*  skipWhitespace
    <*  token ')'
    <?> "application"

  int <- rule $ (\firstDigit rest -> Int $ pack $ firstDigit:rest)
    <$> satisfy (`elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9'])
    <*> many (satisfy isDigit)
    <?> "integer"

  return (int <|> app)
