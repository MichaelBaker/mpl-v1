module Mpl.ParserDescription where

import Text.PrettyPrint.ANSI.Leijen as Pretty hiding ((<>), (<$>))

data ParserDescription =
    NameDescription
    { name :: String }
  | RichDescription
    { name     :: String
    , examples :: [String]
    }
  deriving (Show, Eq)

instance Ord ParserDescription where
  compare a b = compare (name a) (name b)
