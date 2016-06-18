module Mpl.ASTToCore where

import Mpl.AST  (AST(..))
import Mpl.Core (Core(..))

astToCore = elaborate [0]

elaborate :: [Int] -> AST -> Core
elaborate path (AInt value) = CInt path value
elaborate path (ASym value) = CIdent path value
elaborate path (ASexp "(" ")" [ASym "#", ASexp "[" "]" params, body]) = elaborateFunc path params body
elaborate path (ASexp "(" ")" (f:args)) = curryApplication path f $ reverse args
elaborate _ a = error $ "Invalid s-expression: " ++ show a

elaborateFunc path [] body         = CThunk path (elaborate (0:path) body)
elaborateFunc path (sym:[]) body   = CFunc path (symName sym) (elaborate (0:path) body)
elaborateFunc path (sym:rest) body = CFunc path (symName sym) $ elaborateFunc (0:path) rest body

curryApplication path f []     = CForce path (elaborate (0:path) f)
curryApplication path f (a:[]) = CApp path (elaborate (0:path) f) (elaborate (1:path) a)
curryApplication path f (a:as) = CApp path (curryApplication (0:path) f as) (elaborate (1:path) a)

symName (ASym name) = name
symName a           = error $ "Invalid parameter name: " ++ show a
