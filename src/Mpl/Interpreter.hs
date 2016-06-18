module Mpl.Interpreter where

import Mpl.Core  (Core(..))
import Data.Text (Text)

import qualified Data.Map.Strict as Map

type Env = Map.Map Text Val

data Val
  = Core Core
  | Closure Env Core
  deriving (Show)

data RuntimeError
  = AppliedNonFunction Core
  | AppliedNonThunk Core
  | UnboundIdentifier Text
  deriving (Show)

interpret :: Core -> Either RuntimeError Val
interpret ast = exec Map.empty ast

exec :: Env -> Core -> Either RuntimeError Val
exec env (CIdent path a) = case Map.lookup a env of
                             Nothing -> Left $ UnboundIdentifier a
                             Just o  -> Right o
exec env c@(CThunk _ _)   = Right $ Closure env c
exec env c@(CFunc  _ _ _) = Right $ Closure env c
exec env (CForce _ a)     = do
  a' <- exec env a
  forceThunk a'
exec env (CApp   _ f a)   = do
  f' <- exec env f
  a' <- exec env a
  evalFunction f' a'
exec env a = Right $ Core a

forceThunk (Closure closedEnv (CThunk _ body)) = exec closedEnv body
forceThunk (Closure _ c) = Left $ AppliedNonThunk c
forceThunk (Core c) = Left $ AppliedNonThunk c

evalFunction (Closure closedEnv (CFunc _ param body)) arg = exec (Map.insert param arg closedEnv) body
evalFunction (Closure _ c) _ = Left $ AppliedNonFunction c
evalFunction (Core c) _ = Left $ AppliedNonFunction c
