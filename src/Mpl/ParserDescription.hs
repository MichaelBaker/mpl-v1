module Mpl.ParserDescription where

import Mpl.Prelude
import Text.Trifecta.Delta (Delta)

data ParserDescription =
  ParserDescription
    { parserName        :: String
    , parserExamples    :: [String]
    , parserDelta       :: Delta
    , parserExpectation :: String
    }
  deriving (Show, Eq)

instance Ord ParserDescription where
  compare a b = compare (parserName a) (parserName b)
