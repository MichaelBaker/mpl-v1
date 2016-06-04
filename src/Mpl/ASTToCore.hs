module Mpl.ASTToCore where

import Mpl.AST (AST(..), Core(..), CoreType(..), typeOf)


astToCore e ast = elaborate e [0] ast

-- |e is an empty "environment" to attach to each closure that gets created
-- [Int] is a unique identifier for the Core node and a path through the tree that tells you how to get to that node
elaborate :: (Show e) => e -> [Int] -> AST () -> Core [Int] e
elaborate _ path (AInt    _ value)          = CInt   path value
elaborate _ path (AFloat  _ value)          = CReal  path value
elaborate _ path (AText   _ value)          = CText  path value
elaborate _ path (ASym    _ value)          = CIdent path value
elaborate e path (ASexp   _ "[" "]" values) = CList  path (map (\(i, v) -> elaborate e (i:path) v) $ zip [0..] values)
elaborate e path (ASexp   _ "{" "}" values) = CAssoc path (map (\(i, (k, v)) -> (elaborate e (i:path) k, elaborate e ((i + 1):path) v)) $ zip [0,2..] $ listToPairs "map" values)
elaborate e path (ASexp   _ "(" ")" [])     = CUnit  path
elaborate e path (ASexp   _ "(" ")" [ASym _ "#", ASexp _ "[" "]" params, body]) = elaborateFunc   e path params body
elaborate e path (ASexp   _ "(" ")" [ASym _ ":", ASexp _ "[" "]" params, body]) = elaborateTyFunc e path params body
elaborate e path a@(ASexp _ "(" ")" [ASym _ "$", ASexp _ "[" "]" _, _])         = CFOmega path $ elaborateOmega a
elaborate e path (ASexp   _ "(" ")" (f:args)) = curryApplication e path f $ reverse args
elaborate _ _ a = error $ "Invalid s-expression: " ++ show a

elaborateFunc e path [] body       = CThunk path e (elaborate e (0:path) body)
elaborateFunc e path (p:[]) body   = CFunc  path e (convertParam p) (elaborate e (0:path) body)
elaborateFunc e path (p:rest) body = CFunc  path e (convertParam p) $ elaborateFunc e (0:path) rest body

elaborateTyFunc e path []         body = elaborate e path body
elaborateTyFunc e path (sym:[])   body = CTyFunc path e (symName sym) (elaborate e (0:path) body)
elaborateTyFunc e path (sym:rest) body = CTyFunc path e (symName sym) $ elaborateTyFunc e (0:path) rest body

curryApplication e path f [] = case elaborate e (0:path) f of
                                 a@(CFOmega _ _) -> error $ "Type operator applied to no arguments: " ++ show a
                                 t -> CForce path t
curryApplication e path f (a:[]) = case elaborate e (0:path) f of
                                     (CFOmega m b)       -> CFOmega m $ CTApp b (elaborateOmega a)
                                     c@(CTyFunc _ _ _ _) -> CTyApp path c (elaborateOmega a)
                                     b -> CApp path b (elaborate e (1:path) a)
curryApplication e path f (a:as) = case curryApplication e (0:path) f as of
                                     (CFOmega m b)       -> CFOmega m $ CTApp b (elaborateOmega a)
                                     c@(CTyApp  _ _ _)   -> CTyApp path c (elaborateOmega a)
                                     b -> CApp path b (elaborate e (1:path) a)

elaborateOmega (ASym _ value) = convertType value
elaborateOmega (ASexp _ "(" ")" [ASym _ "$", ASexp _ "[" "]" params, body]) = elaborateOmegaFunc params body
elaborateOmega (ASexp _ "(" ")" (f:args)) = curryOmegaApp f $ reverse args
elaborateOmega a = error $ "Invalid ast for type operator: " ++ show a

elaborateOmegaFunc [] body     = elaborateOmega body
elaborateOmegaFunc (a:as) body = CTFOmega (symName a) (elaborateOmegaFunc as body)

curryOmegaApp f []     = error $ "Type operator applied to no arguments: " ++ show f
curryOmegaApp f (a:[]) = CTApp (elaborateOmega f) (elaborateOmega a)
curryOmegaApp f (a:as) = CTApp (curryOmegaApp f as) $ elaborateOmega a

listToPairs _    []         = []
listToPairs item (a:[])     = error $ "Odd number of items in " ++ item
listToPairs item (a:b:rest) = (a, b) : listToPairs item rest

symName (ASym _ name) = name
symName a             = error $ "Invalid parameter name: " ++ show a

convertParam (ASexp _ "(" ")" [ASym _ ":", ASym _ p, ASym _ t]) = (p, convertType t)
convertParam a = error $ "Invalid parameter: " ++ show a

convertType name = case typeOf name of
                     Nothing -> CTSym name
                     Just a  -> a

