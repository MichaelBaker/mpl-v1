module Mpl.AST where

import Data.Text (Text)
import qualified Data.Map as Map

class Meta a where
  meta :: a b -> b

type Env = Map.Map Text (Core [Int])
emptyEnv = Map.empty :: Env

type Context = Map.Map Text  CoreType
emptyContext = Map.empty :: Context

data Core a = CUnit   a
            | CInt    a Integer
            | CReal   a Double
            | CText   a Text
            | CIdent  a Text
            | CList   a [Core a]
            | CAssoc  a [(Core a, Core a)]
            | CMap    a (Map.Map (Core a) (Core a))
            | CThunk  a Env Context (Core a)
            | CFunc   a Env Context (Text, Text) (Core a)
            | CTyFunc a Env Context Text (Core a)
            | CForce  a (Core a)
            | CApp    a (Core a) (Core a)
            deriving (Show, Eq, Ord)

data CoreType = CUnitTy
              | CIntTy
              | CRealTy
              | CTextTy
              | CListTy
              | CMapTy
              | CThunkTy CoreType
              | CFuncTy  CoreType CoreType
              | CUnknownTy
              deriving (Show, Eq, Ord)

instance Meta Core where
  meta (CUnit   a)         = a
  meta (CInt    a _)       = a
  meta (CReal   a _)       = a
  meta (CText   a _)       = a
  meta (CIdent  a _)       = a
  meta (CList   a _)       = a
  meta (CAssoc  a _)       = a
  meta (CMap    a _)       = a
  meta (CThunk  a _ _ _)   = a
  meta (CFunc   a _ _ _ _) = a
  meta (CTyFunc a _ _ _ _) = a
  meta (CForce  a _)       = a
  meta (CApp    a _ _)     = a

data AST a = AInt   a Integer
           | AFloat a Double
           | AText  a Text
           | ASym   a Text
           | ASexp  a Text Text [AST a]
           deriving (Show, Eq)

instance Meta AST where
  meta (AInt   a _)     = a
  meta (AFloat a _)     = a
  meta (AText  a _)     = a
  meta (ASym a _)       = a
  meta (ASexp  a _ _ _) = a
