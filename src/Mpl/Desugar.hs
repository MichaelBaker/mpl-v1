module Mpl.Desugar where

import Mpl.AST (AST(..), ASTType(..), Core(..), CoreType(..))

desugar (AUnit m)                     = CUnit m
desugar (AInt m value)                = CInt m value
desugar (AFloat m value)              = CReal m value
desugar (AText m value)               = CText m value
desugar (AIdent m name)               = CIdent m name
desugar (AList m values)              = CList m (map desugar values)
desugar (AMap m pairs)                = CMap m (map (\(k, v) -> (desugar k, desugar v)) pairs)
desugar (AFunc m [] body)             = CThunk m (desugar body)
desugar (AFunc m (param:[]) body)     = CFunc m (desugarParam param) (desugar body)
desugar (AFunc m (param:params) body) = CFunc m (desugarParam param) $ desugar (AFunc m params body)
desugar (AApp m func [])              = CForce m (desugar func)
desugar (AApp m func args)            = curryApplication m (desugar func) args

desugarParam (name, ty) = (name, desugarType ty)

desugarType AUnitTy  = CUnitTy
desugarType AIntTy   = CIntTy
desugarType AFloatTy = CRealTy
desugarType ATextTy  = CTextTy
desugarType AListTy  = CListTy
desugarType AMapTy   = CMapTy

curryApplication _ inner []     = inner
curryApplication m inner (a:as) = curryApplication m (CApp m inner (desugar a)) as
