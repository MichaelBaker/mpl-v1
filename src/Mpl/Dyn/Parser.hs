{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Mpl.Dyn.AST         (AST(..))
import Text.Earley         ((<?>), Grammar, Report, Prod, satisfy, rule, fullParses)
import Data.Text           (Text, pack)
import Data.Text.Read      (signed, decimal)
import Control.Applicative ((<|>), many, some)
import Data.Char           (isDigit, isSpace)
import qualified Text.Earley as E
import qualified Data.Text   as T

toAST :: Text -> ([AST], Report Text Text)
toAST = fullParses (E.parser grammar)

grammar :: Grammar r (Prod r Text Char (AST))
grammar = mdo
  let exp           = int
      naturalDigits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
      floating    a = spaceAfter a <|> a
      spaceAfter  a = a <* some (satisfy whitespace)
      whitespace    = isSpace

  int <- rule $ (\firstDigit rest -> AInt $ forceRead (signed decimal) $ pack $ firstDigit:rest)
    <$> satisfy (`elem` naturalDigits)
    <*> many (satisfy isDigit)
    <?> "integer"

  return $ floating exp

forceRead reader a = case reader a of
  Left e  -> error e
  Right b -> if T.null (snd b)
    then fst b
    else error ("Unconsumed input: " ++ (show $ snd b))
