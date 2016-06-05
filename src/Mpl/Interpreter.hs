module Mpl.Interpreter where

import Mpl.Core  (Core(..), meta)
import Data.Text (Text, pack)

import qualified Data.Map.Strict as Map

data Env = Env (Map.Map Text (Core [Int] Env)) deriving (Show, Eq, Ord)
emptyEnv = Env $ Map.empty

interpret :: Core [Int] Env -> Core [Int] Env
interpret ast = exec emptyEnv ast

exec :: Env -> Core [Int] Env -> Core [Int] Env
exec (Env env) (CIdent path a) = case Map.lookup a env of
                                   Nothing -> CText path $ pack $ "Invalid identifier: " ++ (show a)
                                   Just o  -> o
exec env (CList   m as)    = CList   m (map (exec env) as)
exec env (CAssoc  m as)    = CMap    m $ Map.fromList $ map (\(k, v) -> (exec env k, exec env v)) as
exec env (CThunk  m _ a)   = CThunk  m env a
exec env (CFunc   m _ a b) = CFunc   m env a b
exec env (CTyFunc m _ a b) = CTyFunc m env a b
exec env (CForce  _ a)     = forceThunk $ exec env a
exec env (CApp    _ f a)   = evalFunction (exec env f) (exec env a)
exec env a                 = a

forceThunk (CThunk _ closedEnv body) = exec closedEnv body
forceThunk ast = CText (meta ast) (pack $ "Tried to force something that isn't a thunk: " ++ show ast)

evalFunction (CFunc   _ closedEnv (param, _) body) arg = exec (binding param arg closedEnv) body
evalFunction (CTyFunc _ closedEnv tyParam body)    arg = exec closedEnv body
evalFunction ast _ = CText (meta ast) (pack $ "Tried to apply something that isn't a function: " ++ show ast)

binding ident val (Env env) = Env $ Map.insert ident val env
