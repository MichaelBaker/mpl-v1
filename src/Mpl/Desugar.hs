module Mpl.Desugar where

import Mpl.AST (AST(..), Core(..), CoreType(..), emptyEnv)

desugar _ = CUnit [0]
-- desugar = desugar' [0]

-- desugar' :: [Int] -> AST () -> Core [Int]
-- desugar' path (AUnit  _)                     = CUnit  path
-- desugar' path (AInt   _ value)               = CInt   path value
-- desugar' path (AFloat _ value)               = CReal  path value
-- desugar' path (AText  _ value)               = CText  path value
-- desugar' path (AIdent _ name)                = CIdent path name
-- desugar' path (AList  _ values)              = CList  path (map (\(i, v) -> desugar' (i:path) v) $ zip [0..] values)
-- desugar' path (AMap   _ pairs)               = CAssoc path (map (\(i, (k, v)) -> (desugar' (i:path) k, desugar' ((i + 1):path) v)) $ zip [0,2..] pairs)
-- desugar' path (AFunc  _ [] body)             = CThunk path emptyEnv (desugar' (0:path) body)
-- desugar' path (AFunc  _ (param:[]) body)     = CFunc  path emptyEnv (desugar'Param param) (desugar' (0:path) body)
-- desugar' path (AFunc  m (param:params) body) = CFunc  path emptyEnv (desugar'Param param) $ desugar' (0:path) (AFunc m params body)
-- desugar' path (AApp   _ func args)           = curryApplication path func $ reverse args
-- 
-- desugar'Param (name, ty) = (name, desugar'Type ty)
-- 
-- desugar'Type AUnitTy  = CUnitTy
-- desugar'Type AIntTy   = CIntTy
-- desugar'Type AFloatTy = CRealTy
-- desugar'Type ATextTy  = CTextTy
-- desugar'Type AListTy  = CListTy
-- desugar'Type AMapTy   = CMapTy
-- 
-- curryApplication path f []     = CForce path (desugar' (0:path) f)
-- curryApplication path f (a:[]) = CApp path (desugar' (0:path) f) (desugar' (1:path) a)
-- curryApplication path f (a:as) = CApp path (curryApplication (0:path) f as) (desugar' (1:path) a)
