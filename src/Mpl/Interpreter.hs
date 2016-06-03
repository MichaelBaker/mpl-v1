module Mpl.Interpreter where

import Mpl.AST   (Core(..), CoreType(..), Env, Context, emptyEnv, emptyContext, meta)
import Data.Text (pack)

import qualified Data.Map.Strict as Map

interpret :: Core [Int] -> Core [Int]
interpret ast = exec emptyEnv emptyContext ast

exec :: Env -> Context -> Core [Int] -> Core [Int]
exec env _ (CIdent path a) = case Map.lookup a env of
  Nothing -> CText path $ pack $ "Invalid identifier: " ++ (show a)
  Just o  -> o
exec env ctx (CList   t as)      = CList t (map (exec env ctx) as)
exec env ctx (CAssoc  t as)      = CMap t $ Map.fromList $ map (\(k, v) -> (exec env ctx k, exec env ctx v)) as
exec env ctx (CThunk  t _ _ a)   = CThunk  t env ctx a
exec env ctx (CFunc   t _ _ a b) = CFunc   t env ctx a b
exec env ctx (CTyFunc t _ _ a b) = CTyFunc t env ctx a b
exec env ctx (CForce  _ a)       = forceThunk $ exec env ctx a
exec env ctx (CApp    _ f a)     = evalFunction (exec env ctx f) (exec env ctx a)
exec env ctx a                   = a

forceThunk (CThunk _ closedEnv closedCtx body) = exec closedEnv closedCtx body
forceThunk ast = CText (meta ast) (pack $ "Tried to force something that isn't a thunk: " ++ show ast)

evalFunction (CFunc   _ closedEnv closedCtx (param, _) body) arg = exec (Map.insert param arg closedEnv) closedCtx body
evalFunction (CTyFunc _ closedEnv closedCtx tyParam body) arg = exec closedEnv (Map.insert tyParam (identToTy arg) closedCtx)  body
evalFunction ast _ = CText (meta ast) (pack $ "Tried to apply something that isn't a function: " ++ show ast)

identToTy (CIdent _ "unit") = CUnitTy
identToTy (CIdent _ "int")  = CIntTy
identToTy (CIdent _ "real") = CRealTy
identToTy (CIdent _ "text") = CTextTy
identToTy (CIdent _ "list") = CListTy
identToTy (CIdent _ "map")  = CMapTy
identToTy a = error $ "Invalid type: " ++ show a
