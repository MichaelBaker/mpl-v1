module Mpl.Interpreter where

import Mpl.Core  (Core(..))
import Data.Text (Text)

import qualified Data.Map.Strict as Map

type Env m = Map.Map Text (Val m)

data Val m
  = Core (Core m)
  | Closure (Env m) (Core m)
  deriving (Show)

data RuntimeError m
  = AppliedNonFunction (Core m)
  | AppliedNonThunk (Core m)
  | UnboundIdentifier Text
  deriving (Show)

toValue :: Core m -> Either (RuntimeError m) (Core m)
toValue ast = case exec Map.empty ast of
  Left e              -> Left e
  Right (Core c)      -> Right c
  Right (Closure _ c) -> Right c

exec :: Env m -> Core m -> Either (RuntimeError m) (Val m)
exec env (CIdent path _ a) = case Map.lookup a env of
                             Nothing -> Left $ UnboundIdentifier a
                             Just o  -> Right o
exec env c@(CThunk _ _ _)   = Right $ Closure env c
exec env c@(CFunc  _ _ _ _) = Right $ Closure env c
exec env (CForce _ _ a)     = do
  a' <- exec env a
  forceThunk a'
exec env (CApp _ _ f a)   = do
  f' <- exec env f
  a' <- exec env a
  evalFunction f' a'
exec env a = Right $ Core a

forceThunk (Closure closedEnv (CThunk _ _ body)) = exec closedEnv body
forceThunk (Closure _ c) = Left $ AppliedNonThunk c
forceThunk (Core c) = Left $ AppliedNonThunk c

evalFunction (Closure closedEnv (CFunc _ _ param body)) arg = exec (Map.insert param arg closedEnv) body
evalFunction (Closure _ c) _ = Left $ AppliedNonFunction c
evalFunction (Core c) _ = Left $ AppliedNonFunction c
