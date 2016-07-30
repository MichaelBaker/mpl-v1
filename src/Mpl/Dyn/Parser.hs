{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Mpl.Dyn.AST         (AST(..))
import Text.Earley         ((<?>), Grammar, Report, Prod, satisfy, rule, fullParses, token)
import Data.Text           (Text, pack)
import Data.Text.Read      (signed, decimal)
import Control.Applicative ((<|>), many, some)
import Data.Char           (isDigit, isSpace, isAsciiLower, isAsciiUpper)
import qualified Text.Earley as E
import qualified Data.Text   as T

data ParseType = Exp | Prog

toAST :: ParseType -> Text -> ([AST], Report Text Text)
toAST parseType = fullParses (E.parser $ grammar parseType)

grammar :: ParseType -> Grammar r (Prod r Text Char (AST))
grammar parseType = mdo
  let exp           = int
      symChars      = ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '\\', '/', '.']
      naturalDigits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
      floating    a = spaceAfter a <|> a
      spaceAfter  a = a <* some (satisfy whitespace)
      whitespace    = isSpace

  prog <- rule $ AProg <$> many def <?> "program"

  int <- rule $ (\firstDigit rest -> AInt $ forceRead (signed decimal) $ pack $ firstDigit:rest)
    <$> satisfy (`elem` naturalDigits)
    <*> many (satisfy isDigit)
    <?> "integer"

  def <- rule $ ADef <$> spaceAfter sym <* spaceAfter (token '=') <*> exp <?> "definition"

  sym <- rule $ (\a as -> ASym $ pack (a:as))
    <$> satisfy (\c -> any ($ c) [isAsciiLower, (`elem` symChars)])
    <*> many (satisfy (\c -> any ($ c) [isDigit, isAsciiLower, isAsciiUpper, (`elem` symChars)]))
    <?> "symbol"

  return $ case parseType of
    Exp  -> floating exp
    Prog -> floating prog

forceRead reader a = case reader a of
  Left e  -> error e
  Right b -> if T.null (snd b)
    then fst b
    else error ("Unconsumed input: " ++ (show $ snd b))
