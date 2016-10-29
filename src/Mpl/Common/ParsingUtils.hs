module Mpl.Common.ParsingUtils
  ( module Mpl.Common.ParsingUtils
  , Text
  , Result(Success, Failure)
  , Parser
  , (<?>)
  , (<|>)
  , integer
  , many
  , some
  , oneOf
  , whiteSpace
  , try
  , parens
  , symbolic
  , optional
  )
where

import Prelude ((++))
import Text.Trifecta.Parser    (Parser, parseString)
import Text.Trifecta.Result    (Result(Success, Failure))
import Text.Trifecta.Delta     (Delta(Columns))
import Text.Parser.Token       (integer, whiteSpace, parens, symbolic)
import Data.Text               (Text, pack, unpack)
import Text.Parser.Combinators ((<?>), try, optional)
import Control.Applicative     ((<|>), many, some)
import Text.Parser.Char        (oneOf)

textToString = unpack
stringToText = pack

parseFromString parser = parseString parser zeroDelta

zeroDelta = Columns 0 0

symbolStartChars =
  ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

upcaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars = symbolStartChars ++ digits

digits = "0123456789"
