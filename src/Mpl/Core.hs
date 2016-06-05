module Mpl.Core where

import Data.Text (Text)
import Data.List (find)
import qualified Data.Map as Map

-- |m is any metadata that might be relevant to the node during a particular compilation phase
-- e represents an "environment". This is where a closure's environment is stored during execution.
data Core m e = CUnit   m
              | CInt    m Integer
              | CReal   m Double
              | CText   m Text
              | CIdent  m Text
              | CList   m [Core m e]
              | CAssoc  m [(Core m e, Core m e)]
              | CMap    m (Map.Map (Core m e) (Core m e))
              | CThunk  m e (Core m e)
              | CFunc   m e (Text, CoreType) (Core m e)
              | CTyFunc m e Text (Core m e)
              | CForce  m (Core m e)
              | CApp    m (Core m e) (Core m e)
              | CTyApp  m (Core m e) CoreType
              | CFOmega m CoreType
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
              | CTSym Text
              | CTFOmega Text CoreType
              | CTApp CoreType CoreType
              deriving (Show, Eq, Ord)

nameOf :: CoreType -> Text
nameOf CUnitTy        = "()"
nameOf CIntTy         = "int"
nameOf CRealTy        = "real"
nameOf CTextTy        = "text"
nameOf CListTy        = "list"
nameOf CMapTy         = "map"
nameOf (CThunkTy _)   = "thunk"
nameOf (CFuncTy  _ _) = "func"
nameOf CTyFuncTy      = "forall"
nameOf CUnknownTy     = "unknown"
nameOf (CTFOmega _ _) = "ct-f-omega"
nameOf (CTApp _ _)    = "ct-app"
nameOf (CTSym _)      = "ct-sym"

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

meta (CUnit   m)       = m
meta (CInt    m _)     = m
meta (CReal   m _)     = m
meta (CText   m _)     = m
meta (CIdent  m _)     = m
meta (CList   m _)     = m
meta (CAssoc  m _)     = m
meta (CMap    m _)     = m
meta (CThunk  m _ _)   = m
meta (CFunc   m _ _ _) = m
meta (CTyFunc m _ _ _) = m
meta (CForce  m _)     = m
meta (CApp    m _ _)   = m
meta (CFOmega m _)     = m
meta (CTyApp  m _ _)   = m
