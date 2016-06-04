module Mpl.ASTToCore where

import Mpl.AST (AST(..), Core(..), CoreType(..))


astToCore e ast = elaborate e [0] ast

elaborate :: e -> [Int] -> AST () -> Core [Int] e
elaborate _ path (AInt   _ value)          = CInt   path value
elaborate _ path (AFloat _ value)          = CReal  path value
elaborate _ path (AText  _ value)          = CText  path value
elaborate _ path (ASym   _ value)          = CIdent path value
elaborate e path (ASexp  _ "[" "]" values) = CList  path (map (\(i, v) -> elaborate e (i:path) v) $ zip [0..] values)
elaborate e path (ASexp  _ "{" "}" values) = CAssoc path (map (\(i, (k, v)) -> (elaborate e (i:path) k, elaborate e ((i + 1):path) v)) $ zip [0,2..] $ listToPairs "map" values)
elaborate e path (ASexp  _ "(" ")" [])     = CUnit  path
elaborate e path (ASexp  _ "(" ")" [ASym _ "#", ASexp _ "[" "]" params, body]) = elaborateFunc   e path params body
elaborate e path (ASexp  _ "(" ")" [ASym _ ":", ASexp _ "[" "]" params, body]) = elaborateTyFunc e path params body
elaborate e path (ASexp  _ "(" ")" (f:args)) = curryApplication e path f $ reverse args
elaborate _ _ a = error $ "Invalid s-expression: " ++ show a

elaborateFunc e path [] body               = CThunk path e (elaborate e (0:path) body)
elaborateFunc e path (p:[]) body   = CFunc  path e (convertParam p) (elaborate e (0:path) body)
elaborateFunc e path (p:rest) body = CFunc  path e (convertParam p) $ elaborateFunc e (0:path) rest body

elaborateTyFunc e path []         body = CThunk  path e (elaborate e (0:path) body)
elaborateTyFunc e path (sym:[])   body = CTyFunc path e (symName sym) (elaborate e (0:path) body)
elaborateTyFunc e path (sym:rest) body = CTyFunc path e (symName sym) $ elaborateTyFunc e (0:path) rest body

curryApplication e path f []     = CForce path (elaborate e (0:path) f)
curryApplication e path f (a:[]) = CApp   path (elaborate e (0:path) f) (elaborate e (1:path) a)
curryApplication e path f (a:as) = CApp   path (curryApplication e (0:path) f as) (elaborate e (1:path) a)

listToPairs _    []         = []
listToPairs item (a:[])     = error $ "Odd number of items in " ++ item
listToPairs item (a:b:rest) = (a, b) : listToPairs item rest

symName (ASym _ name) = name
symName a             = error $ "Invalid parameter name: " ++ show a

convertParam (ASexp _ "(" ")" [ASym _ ":", ASym _ p, ASym _ t]) = (p, t)
convertParam a = error $ "Invalid parameter: " ++ show a
