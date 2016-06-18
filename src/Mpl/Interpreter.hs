module Mpl.Interpreter where

import Mpl.AST   (Core(..), Env, emptyEnv, meta)
import Data.Text (pack)

import qualified Data.Map.Strict as Map

interpret :: Core [Int] -> Core [Int]
interpret ast = exec emptyEnv ast

exec :: Env -> Core [Int] -> Core [Int]
exec env (CIdent path a) = case Map.lookup a env of
  Nothing -> error $ "Invalid identifier: " ++ (show a)
  Just o  -> o
exec env (CThunk t _ a)   = CThunk t env a
exec env (CFunc  t _ a b) = CFunc  t env a b
exec env (CForce _ a)     = forceThunk $ exec env a
exec env (CApp   _ f a)   = evalFunction (exec env f) (exec env a)
exec env a                = a

forceThunk (CThunk _ closedEnv body) = exec closedEnv body
forceThunk ast = error $ "Tried to force something that isn't a thunk: " ++ show ast

evalFunction (CFunc _ closedEnv param body) arg = exec (Map.insert param arg closedEnv) body
evalFunction ast _ = error $ "Tried to apply something that isn't a function: " ++ show ast
