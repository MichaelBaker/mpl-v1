{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Control.Applicative ((<|>), many, some, optional)
import Data.Char           (isSpace, isDigit, isAscii, isLetter, isAsciiUpper, isAsciiLower)
import Data.Text           (Text, pack)
import Text.Earley         ((<?>), Grammar, Report, Prod, list, satisfy, rule, fullParses, token, listLike)

import qualified Text.Earley as E

data AST a = AInt    a Text
           | AFloat  a Text
           | AText   a Text
           | AIdent  a Text
           | AList   a [AST a]
           | AMap    a [(AST a, AST a)]
           | AFunc   a (AST a) (AST a)
           | AApp    a (AST a) [AST a]
           deriving (Show, Eq)

parse :: Text -> ([AST ()], Report Text Text)
parse = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Char (AST ()))
grammar = mdo
  let exp           = int <|> float <|> text <|> ident <|> list <|> func <|> application <|> map
      whitespace    = isSpace
      skipManySpace = many (satisfy whitespace)
      spaceBefore a = some (satisfy whitespace) *> a
      spaceAfter  a = a <* some (satisfy whitespace)
      floating    a = spaceAfter a <|> a
      floatingExp   = floating exp
      identSymbols  = ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '\\', '/', '.']
      naturalDigits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
      maybeFollowing r separator = spaceAfter r <|> (r <* skipManySpace <* separator <* skipManySpace)
      separated cons es Nothing  = cons es
      separated cons es (Just l) = cons (es ++ [l])

  map <- rule $ pure (\es last -> case last of Nothing -> AMap () es; Just l -> AMap () (es ++ [l]))
    <*  floating (token '{')
    <*> many (maybeFollowing mapPair (token ','))
    <*> optional mapPair
    <*  token '}'
    <?> "map"

  mapPair <- rule $ (,)
    <$> maybeFollowing exp (token ':')
    <*> floating exp
    <?> "mapPair"

  application <- rule $ pure (separated . (AApp ()))
    <* floating (token '(')
    <*> spaceAfter exp
    <*> many (spaceAfter exp)
    <*> optional exp
    <* token ')'
    <?> "application"

  func <- rule $ pure (AFunc ())
    <* floating (listLike ("#(" :: Text))
    <*> floating list
    <*> floatingExp
    <* token ')'
    <?> "function"

  list <- rule $ pure (separated $ AList ())
    <*  floating (token '[')
    <*> many (maybeFollowing exp (token ','))
    <*> optional exp
    <*  token ']'
    <?> "list"

  ident <- rule $ (\a as -> AIdent () $ pack (a:as))
    <$> satisfy (\c -> any ($ c) [isAsciiLower, (`elem` identSymbols)])
    <*> many (satisfy (\c -> any ($ c) [isDigit, isAsciiLower, isAsciiUpper, (`elem` identSymbols)]))
    <?> "identifier"

  text <- rule $ pure (AText () . pack)
    <*  token '"'
    <*> many (satisfy (/= '"'))
    <*  token '"'
    <?> "text"

  float <- rule $ (\whole dot fraction -> AFloat () $ pack $ whole ++ [dot] ++ fraction)
    <$> (
      some (token '0') <|>
      (\firstDigit rest -> firstDigit:rest)
        <$> satisfy (`elem` naturalDigits)
        <*> many (satisfy isDigit)
    )
    <*> satisfy (== '.')
    <*> some (satisfy isDigit)
    <?> "float"

  int <- rule $ (\firstDigit rest -> AInt () $ pack $ firstDigit:rest)
    <$> satisfy (`elem` naturalDigits)
    <*> many (satisfy isDigit)
    <?> "integer"

  return $ spaceBefore floatingExp <|> floatingExp
