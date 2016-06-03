module Mpl.ASTToCore where

import Mpl.AST (AST(..), Core(..), CoreType(..), emptyEnv, emptyContext)


astToCore = elaborate [0]

elaborate :: [Int] -> AST () -> Core [Int]
elaborate path (AInt   _ value)          = CInt path value
elaborate path (AFloat _ value)          = CReal path value
elaborate path (AText  _ value)          = CText path value
elaborate path (ASym   _ value)          = CIdent path value
elaborate path (ASexp  _ "[" "]" values) = CList  path (map (\(i, v) -> elaborate (i:path) v) $ zip [0..] values)
elaborate path (ASexp  _ "{" "}" values) = CAssoc path (map (\(i, (k, v)) -> (elaborate (i:path) k, elaborate ((i + 1):path) v)) $ zip [0,2..] $ listToPairs "map" values)
elaborate path (ASexp  _ "(" ")" [])     = CUnit  path
elaborate path (ASexp  _ "(" ")" [ASym _ "#", ASexp _ "[" "]" params, body]) = elaborateFunc   path (listToPairs "parameter list" params) body
elaborate path (ASexp  _ "(" ")" [ASym _ ":", ASexp _ "[" "]" params, body]) = elaborateTyFunc path params body
elaborate path (ASexp  _ "(" ")" (f:args)) = curryApplication path f $ reverse args
elaborate _ a = error $ "Invalid s-expression: " ++ show a

elaborateFunc path [] body               = CThunk path emptyEnv emptyContext (elaborate (0:path) body)
elaborateFunc path ((sym, ty):[]) body   = CFunc path emptyEnv emptyContext (symName sym, symName ty) (elaborate (0:path) body)
elaborateFunc path ((sym, ty):rest) body = CFunc path emptyEnv emptyContext (symName sym, symName ty) $ elaborateFunc (0:path) rest body

elaborateTyFunc path []         body = CThunk  path emptyEnv emptyContext (elaborate (0:path) body)
elaborateTyFunc path (sym:[])   body = CTyFunc path emptyEnv emptyContext (symName sym) (elaborate (0:path) body)
elaborateTyFunc path (sym:rest) body = CTyFunc path emptyEnv emptyContext (symName sym) $ elaborateTyFunc (0:path) rest body

curryApplication path f []     = CForce path (elaborate (0:path) f)
curryApplication path f (a:[]) = CApp path (elaborate (0:path) f) (elaborate (1:path) a)
curryApplication path f (a:as) = CApp path (curryApplication (0:path) f as) (elaborate (1:path) a)

listToPairs _    []         = []
listToPairs item (a:[])     = error $ "Odd number of items in " ++ item
listToPairs item (a:b:rest) = (a, b) : listToPairs item rest

symName (ASym _ name) = name
symName a             = error $ "Invalid parameter name: " ++ show a

symToType (ASym _ "unit")  = CUnitTy
symToType (ASym _ "int")   = CIntTy
symToType (ASym _ "float") = CRealTy
symToType (ASym _ "text")  = CTextTy
symToType (ASym _ "list")  = CListTy
symToType (ASym _ "map")   = CMapTy
symToType a                = error $ "Invalid type name: " ++ show a
