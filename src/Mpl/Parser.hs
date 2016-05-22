{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Control.Applicative ((<|>), many, some, optional)
import Data.Char           (isSpace, isDigit, isAsciiUpper, isAsciiLower)
import Data.Text           (Text, pack)
import Text.Earley         ((<?>), Grammar, Report, Prod, list, satisfy, rule, fullParses, token, listLike)

import qualified Text.Earley as E

data AST = AApp   AST [AST]
         | AInt   Text
         | AFloat Text
         | AIdent Text
         | AList  [AST]
         deriving (Show, Eq)

parse :: Text -> ([AST], Report Text Text)
parse = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Char AST)
grammar = mdo
  let exp           = int <|> float <|> list
      whitespace  a = isSpace a || a == ','
      spaceBefore a = some (satisfy whitespace) *> a
      spaceAfter  a = a <* some (satisfy whitespace)
      floating    a = spaceAfter a <|> a

  list <- rule $ pure (\es last -> case last of Nothing -> AList es; Just l -> AList (es ++ [l]))
    <*  floating (token '[')
    <*> many (spaceAfter exp)
    <*> optional (exp)
    <*  token ']'
    <?> "list"

  float <- rule $ (\whole dot fraction -> AFloat $ pack $ whole ++ [dot] ++ fraction)
    <$> (
      some (token '0') <|>
      (\firstDigit rest -> firstDigit:rest)
        <$> satisfy (`elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9'])
        <*> many (satisfy isDigit)
    )
    <*> satisfy (== '.')
    <*> some (satisfy isDigit)
    <?> "float"

  int <- rule $ (\firstDigit rest -> AInt $ pack $ firstDigit:rest)
    <$> satisfy (`elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9'])
    <*> many (satisfy isDigit)
    <?> "integer"

  return $ spaceBefore exp <|> exp

-- app <- rule $ pure AApp
--   <*  token '('
--   <*> floatingExp
--   <*> (some floatingExp)
--   <*  token ')'
--   <?> "application"

-- ident <- rule $ (\firstLetter rest -> AIdent $ pack $ firstLetter:rest)
--   <$> satisfy isAsciiLower
--   <*> many (satisfy $ \a -> any ($ a) [isAsciiLower, isAsciiUpper, isDigit])
--   <?> "identifier"
