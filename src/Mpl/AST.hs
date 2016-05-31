module Mpl.AST where

import Data.Text (Text)

data AST a = AInt    a Integer
           | AFloat  a Double
           | AText   a Text
           | AIdent  a Text
           | AList   a [AST a]
           | AMap    a [(AST a, AST a)]
           | AFunc   a [Text] (AST a)
           | ACFunc  a (Maybe Text) (AST a)
           | AApp    a (AST a) [AST a]
           | ACApp   a (AST a) (Maybe (AST a))
           deriving (Show, Eq)

data Type = IntType
          | FloatType
          | TextType
          | IdentType
          | ListType Type
          | MapType  Type Type
          | FuncType Type Type
          | Unknown
          deriving (Show, Eq)

meta (AInt    a _)   = a
meta (AFloat  a _)   = a
meta (AText   a _)   = a
meta (AIdent  a _)   = a
meta (AList   a _)   = a
meta (AMap    a _)   = a
meta (AFunc   a _ _) = a
meta (ACFunc  a _ _) = a
meta (AApp    a _ _) = a
meta (ACApp   a _ _) = a
