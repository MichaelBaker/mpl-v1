module Mpl.Common.ParsingUtils
  ( (<?>)
  , (<|>)
  , parseFromString
  , Result(Success, Failure)
  , integer
  , textToString
  , stringToText
  , many
  , some
  , oneOf
  , whiteSpace
  , try
  , parens
  )
where

import Prelude ()
import Text.Trifecta.Parser    (parseString)
import Text.Trifecta.Result    (Result(Success, Failure))
import Text.Trifecta.Delta     (Delta(Columns))
import Text.Parser.Token       (integer, whiteSpace, parens)
import Data.Text               (pack, unpack)
import Text.Parser.Combinators ((<?>), try)
import Control.Applicative     ((<|>), many, some)
import Text.Parser.Char        (oneOf)

textToString = unpack
stringToText = pack

parseFromString parser = parseString parser zeroDelta

zeroDelta = Columns 0 0
