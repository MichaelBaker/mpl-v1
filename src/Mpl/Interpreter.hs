module Mpl.Interpreter where

import Mpl.AST   (Core(..), CoreType(..), Env, emptyEnv, meta)
import Data.Text (pack)

import qualified Data.Map.Strict as Map

interpret :: Core CoreType -> Core CoreType
interpret ast = exec emptyEnv ast

exec :: Env -> Core CoreType -> Core CoreType
exec env (CIdent _ a) = case Map.lookup a env of
  Nothing -> CText CTextTy $ pack $ "Invalid identifier: " ++ (show a)
  Just o  -> o
exec env (CList  t as)    = CList t (map (exec env) as)
exec env (CAssoc t as)    = CMap t $ Map.fromList $ map (\(k, v) -> (exec env k, exec env v)) as
exec env (CThunk t _ a)   = CThunk t env a
exec env (CFunc  t _ a b) = CFunc  t env a b
exec env (CForce _ a)     = forceThunk $ exec env a
exec env (CApp   _ f a)   = evalFunction (exec env f) (exec env a)
exec env a                = a

forceThunk (CThunk _ closedEnv body) = exec closedEnv body
forceThunk ast = CText CTextTy (pack $ "Tried to force something that isn't a thunk: " ++ show ast)

evalFunction (CFunc _ closedEnv (param, _) body) arg = exec (Map.insert param arg closedEnv) body
evalFunction ast _ = CText CTextTy (pack $ "Tried to apply something that isn't a function: " ++ show ast)
