module Mpl.Interpreter where

import Mpl.Core  (Core(..))
import Data.Text (Text)

import qualified Data.Map.Strict as Map

type Env = Map.Map Text Val

data Val = Core (Core [Int])
         | Closure Env (Core [Int])
         deriving (Show)

interpret :: Core [Int] -> Val
interpret ast = exec Map.empty ast

exec :: Env -> Core [Int] -> Val
exec env (CIdent path a) = case Map.lookup a env of
                             Nothing -> error $ "Invalid identifier: " ++ (show a)
                             Just o  -> o
exec env c@(CThunk _ _)   = Closure env c
exec env c@(CFunc  _ _ _) = Closure env c
exec env (CForce _ a)     = forceThunk $ exec env a
exec env (CApp   _ f a)   = evalFunction (exec env f) (exec env a)
exec env a                = Core a

forceThunk (Closure closedEnv (CThunk _ body)) = exec closedEnv body
forceThunk val = error $ "Tried to force something that isn't a thunk: " ++ show val

evalFunction (Closure closedEnv (CFunc _ param body)) arg = exec (Map.insert param arg closedEnv) body
evalFunction val _ = error $ "Tried to apply something that isn't a function: " ++ show val
