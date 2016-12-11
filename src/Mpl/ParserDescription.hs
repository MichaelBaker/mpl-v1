module Mpl.ParserDescription where

data ParserDescription =
    NameDescription
    { name :: String }
  | RichDescription
    { name     :: String
    , examples :: [String]
    }
  deriving (Show, Eq)

data ParserSuggestion =
  ParserSuggestion
  { itemName    :: String
  , expectation :: String
  , example     :: String
  }
  deriving (Show, Eq)

instance Ord ParserDescription where
  compare a b = compare (name a) (name b)
