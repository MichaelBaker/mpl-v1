module Mpl.AST where

import Data.Text (Text)

data AST
  = AInt   Integer
  | ASym   Text
  | ASexp  Text Text [AST]
  deriving (Show, Eq)
