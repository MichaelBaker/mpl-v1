module Mpl.AST where

import Data.Text (Text)

data AST a = AInt   a Integer
           | AFloat a Double
           | AText  a Text
           | ASym   a Text
           | ASexp  a Text Text [AST a]
           deriving (Show, Eq)

meta (AInt   a _)     = a
meta (AFloat a _)     = a
meta (AText  a _)     = a
meta (ASym a _)       = a
meta (ASexp  a _ _ _) = a
