module Mpl.AST where

import Data.Text (Text)
import Data.List (find)
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

transform f a = f (defaultTransform f) a

defaultTransform f a@(CList   m values)   = CList   m     $ map (transform f) values
defaultTransform f a@(CAssoc  m pairs)    = CAssoc  m     $ map (\(k, v) -> (transform f k, transform f v)) pairs
defaultTransform f a@(CMap    m pairs)    = CMap    m     $ Map.foldlWithKey' (\m k v -> Map.insert (transform f k) (transform f v) m) Map.empty pairs
defaultTransform f a@(CThunk  m e body)   = CThunk  m e   $ transform f body
defaultTransform f a@(CFunc   m e p body) = CFunc   m e p $ transform f body
defaultTransform f a@(CTyFunc m e p body) = CTyFunc m e p $ transform f body
defaultTransform f a@(CForce  m thunk)    = CForce  m     $ transform f thunk
defaultTransform f a@(CApp    m func arg) = CApp    m (transform f func) (transform f arg)
defaultTransform _ a                      = a

data CoreType = CUnitTy
              | CIntTy
              | CRealTy
              | CTextTy
              | CListTy
              | CMapTy
              | CThunkTy CoreType
              | CFuncTy  CoreType CoreType
              | CTyFuncTy
              | CUnknownTy
              deriving (Show, Eq, Ord)

nameOf :: CoreType -> Text
nameOf CUnitTy        = "unit"
nameOf CIntTy         = "int"
nameOf CRealTy        = "real"
nameOf CTextTy        = "text"
nameOf CListTy        = "list"
nameOf CMapTy         = "map"
nameOf (CThunkTy _)   = "thunk"
nameOf (CFuncTy  _ _) = "func"
nameOf CTyFuncTy      = "forall"
nameOf CUnknownTy     = "unknown"

typeNames = [
  (nameOf CUnitTy,    CUnitTy),
  (nameOf CIntTy,     CIntTy),
  (nameOf CRealTy,    CRealTy),
  (nameOf CTextTy,    CTextTy),
  (nameOf CListTy,    CListTy),
  (nameOf CMapTy,     CMapTy),
  (nameOf CUnknownTy, CUnknownTy)
  ]

typeOf name = case find ((== name) . fst) typeNames of
                Nothing -> Nothing
                Just a  -> Just $ snd a

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
