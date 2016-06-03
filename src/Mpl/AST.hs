module Mpl.AST where

import Data.Text (Text)
import qualified Data.Map as Map

data Core m a = CUnit   m
              | CInt    m Integer
              | CReal   m Double
              | CText   m Text
              | CIdent  m Text
              | CList   m [Core m a]
              | CAssoc  m [(Core m a, Core m a)]
              | CMap    m (Map.Map (Core m a) (Core m a))
              | CThunk  m a (Core m a)
              | CFunc   m a (Text, Text) (Core m a)
              | CTyFunc m a Text (Core m a)
              | CForce  m (Core m a)
              | CApp    m (Core m a) (Core m a)
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

metaC (CUnit   m)       = m
metaC (CInt    m _)     = m
metaC (CReal   m _)     = m
metaC (CText   m _)     = m
metaC (CIdent  m _)     = m
metaC (CList   m _)     = m
metaC (CAssoc  m _)     = m
metaC (CMap    m _)     = m
metaC (CThunk  m _ _)   = m
metaC (CFunc   m _ _ _) = m
metaC (CTyFunc m _ _ _) = m
metaC (CForce  m _)     = m
metaC (CApp    m _ _)   = m

data AST a = AInt   a Integer
           | AFloat a Double
           | AText  a Text
           | ASym   a Text
           | ASexp  a Text Text [AST a]
           deriving (Show, Eq)

metaA (AInt   a _)     = a
metaA (AFloat a _)     = a
metaA (AText  a _)     = a
metaA (ASym a _)       = a
metaA (ASexp  a _ _ _) = a
