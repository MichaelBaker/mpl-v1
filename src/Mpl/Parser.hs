{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Control.Applicative ((<|>), many)
import Data.Char           (isSpace)
import Data.Text           (Text)
import Text.Earley         ((<?>), Grammar, Report, Prod, satisfy, rule, fullParses, token, listLike)

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
  int <- rule $ Int <$> listLike "1" <?> "integer"
  return (int <|> app)
