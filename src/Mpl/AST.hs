module Mpl.AST where

import Data.Text (Text)

data AST a = AInt    a Integer
           | AFloat  a Double
           | AText   a Text
           | AIdent  a Text
           | AList   a [AST a]
           | AMap    a [(AST a, AST a)]
           | AFunc   a (AST a) (AST a)
           | AApp    a (AST a) [AST a]
           deriving (Show, Eq)

meta (AInt    a _)   = a
meta (AFloat  a _)   = a
meta (AText   a _)   = a
meta (AIdent  a _)   = a
meta (AList   a _)   = a
meta (AMap    a _)   = a
meta (AFunc   a _ _) = a
meta (AApp    a _ _) = a
