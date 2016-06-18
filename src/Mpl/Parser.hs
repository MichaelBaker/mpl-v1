{-# LANGUAGE RecursiveDo #-}

module Mpl.Parser where

import Mpl.AST (AST(..))

import Control.Applicative ((<|>), many, some, optional)
import Data.Char           (isSpace, isDigit, isAsciiUpper, isAsciiLower)
import Data.Text           (Text, pack)
import Data.Text.Read      (signed, decimal)
import Text.Earley         ((<?>), Grammar, Report, Prod, satisfy, rule, fullParses, token)

import qualified Text.Earley as E
import qualified Data.Text   as T

parse :: Text -> ([AST ()], Report Text Text)
parse = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Char (AST ()))
grammar = mdo
  let exp           = int <|> sym <|> sexp
      whitespace    = isSpace
      spaceBefore a = some (satisfy whitespace) *> a
      spaceAfter  a = a <* some (satisfy whitespace)
      floating    a = spaceAfter a <|> a
      symChars      = ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '\\', '/', '.']
      naturalDigits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
      separated cons es Nothing  = cons es
      separated cons es (Just l) = cons (es ++ [l])

  sexp <- rule $ paren <|> square <?> "sexp"

  paren <- rule $ pure (separated $ ASexp () "(" ")")
    <*  floating (token '(')
    <*> many (spaceAfter exp)
    <*> optional exp
    <*  token ')'
    <?> "paren-brackets"

  square <- rule $ pure (separated $ ASexp () "[" "]")
    <*  floating (token '[')
    <*> many (spaceAfter exp)
    <*> optional exp
    <*  token ']'
    <?> "square-brackets"

  sym <- rule $ (\a as -> ASym () $ pack (a:as))
    <$> satisfy (\c -> any ($ c) [isAsciiLower, (`elem` symChars)])
    <*> many (satisfy (\c -> any ($ c) [isDigit, isAsciiLower, isAsciiUpper, (`elem` symChars)]))
    <?> "symbol"

  int <- rule $ (\firstDigit rest -> AInt () $ forceRead (signed decimal) $ pack $ firstDigit:rest)
    <$> satisfy (`elem` naturalDigits)
    <*> many (satisfy isDigit)
    <?> "integer"

  return $ spaceBefore (floating exp) <|> (floating exp)

forceRead reader a = case reader a of
  Left e  -> error e
  Right b -> if T.null (snd b)
    then fst b
    else error ("Unconsumed input: " ++ (show $ snd b))
