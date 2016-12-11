module Mpl.Common.ParsingUtils
  ( module Mpl.Common.ParsingUtils
  , Text
  , Result(Success, Failure)
  , Parser
  , (<?>)
  , (<|>)
  , many
  , some
  , oneOf
  , whiteSpace
  , try
  , parens
  , symbolic
  , optional
  , symbol
  , notFollowedBy
  , char
  )
where

import Control.Applicative     ((<|>), many, some)
import Data.Text               (Text, pack, unpack)
import Mpl.Utils               (textToString, stringToText)
import Prelude                 ((++))
import Text.Parser.Char        (oneOf, char)
import Text.Parser.Combinators ((<?>), try, optional, notFollowedBy)
import Text.Parser.Token       (whiteSpace, parens, symbolic, symbol)
import Text.Trifecta.Delta     (Delta(Columns))
import Text.Trifecta.Parser    (Parser, parseString)
import Text.Trifecta.Result    (Result(Success, Failure))

parseFromString parser = parseString parser zeroDelta

zeroDelta = Columns 0 0

symbolStartChars =
  ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

upcaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars = symbolStartChars ++ digits

digits = "0123456789"
