{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Control.Applicative ((<|>), many, some, optional)
import Data.Char           (isSpace, isDigit, isAsciiUpper, isAsciiLower)
import Data.Text           (Text, pack)
import Text.Earley         ((<?>), Grammar, Report, Prod, list, satisfy, rule, fullParses, token, listLike)

import qualified Text.Earley as E

data AST = AApp    AST [AST]
         | AList   [AST]
         | AMap    [(AST, AST)]
         | AIdent  Text
         | AText   Text
         | AFloat  Text
         | AInt    Text
         deriving (Show, Eq)

parse :: Text -> ([AST], Report Text Text)
parse = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Char AST)
grammar = mdo
  let exp           = int <|> float <|> text <|> list <|> amap
      whitespace    = isSpace
      skipManySpace = many (satisfy whitespace)
      spaceBefore a = some (satisfy whitespace) *> a
      spaceAfter  a = a <* some (satisfy whitespace)
      floating    a = spaceAfter a <|> a
      floatingExp   = floating exp
      maybeFollowing r separator = spaceAfter r <|> (r <* skipManySpace <* separator <* skipManySpace)

  list <- rule $ pure (\es last -> case last of Nothing -> AList es; Just l -> AList (es ++ [l]))
    <*  floating (token '[')
    <*> many (spaceAfter exp <|> (exp <* skipManySpace <* token ',' <* skipManySpace))
    <*> optional exp
    <*  token ']'
    <?> "list"

  amap <- rule $ pure (\es last -> case last of Nothing -> AMap es; Just l -> AMap (es ++ [l]))
    <*  floating (token '{')
    <*> many (maybeFollowing mapPair (token ','))
    <*> optional mapPair
    <*  token '}'
    <?> "amap"

  mapPair <- rule $ (,)
    <$> maybeFollowing exp (token ':')
    <*> floating exp
    <?> "mapPair"

  text <- rule $ pure (AText . pack)
    <*  token '"'
    <*> many (satisfy (/= '"'))
    <*  token '"'

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

  return $ spaceBefore floatingExp <|> floatingExp
