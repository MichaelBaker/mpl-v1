module Mpl.AST where

import Data.Text (Text)

data AST = AInt   Integer
         | AFloat Double
         | AText  Text
         | ASym   Text
         | ASexp  Text Text [AST]
         deriving (Show, Eq)
