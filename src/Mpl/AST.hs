module Mpl.AST where

import Data.Text (Text)

class Meta a where
  meta :: a b -> b


data Core a = CInt   a Integer
            | CReal  a Double
            | CText  a Text
            | CFunc  a Text (Core a)
            | CThunk a (Core a)
            | CApp   a (Core a) (Core a)
            | CExec  a (Core a)
            deriving (Show, Eq)

data CoreType = CIntTy
              | CRealTy
              | CTextTy
              | CFuncTy Type Type
              | CUnknown
              deriving (Show, Eq)

instance Meta Core where
  meta (CInt    a _)   = a
  meta (CReal   a _)   = a
  meta (CText   a _)   = a
  meta (CFunc   a _ _) = a
  meta (CThunk  a _)   = a
  meta (CApp    a _ _) = a
  meta (CExec   a _)   = a

data AST a = AInt    a Integer
           | AFloat  a Double
           | AText   a Text
           | AIdent  a Text
           | AList   a [AST a]
           | AMap    a [(AST a, AST a)]
           | AFunc   a [Text] (AST a)
           | AApp    a (AST a) [AST a]
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

instance Meta AST where
  meta (AInt    a _)   = a
  meta (AFloat  a _)   = a
  meta (AText   a _)   = a
  meta (AIdent  a _)   = a
  meta (AList   a _)   = a
  meta (AMap    a _)   = a
  meta (AFunc   a _ _) = a
  meta (AApp    a _ _) = a
