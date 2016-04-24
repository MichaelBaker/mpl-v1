{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Control.Applicative
import Data.Text   (Text)
import Text.Earley ((<?>), Grammar, Report, Prod, rule, fullParses, token)

import qualified Text.Earley as E

data AST = App Text AST
         | Int Text
         deriving (Show, Eq)

parse :: [Text] -> ([AST], Report Text [Text])
parse = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Text AST)
grammar = mdo
  app <- rule $ App <$> token "f" <*> int <* token ")" <?> "application"
  int <- rule $ Int <$> token "1" <?> "integer"
  return (int <|> app)
