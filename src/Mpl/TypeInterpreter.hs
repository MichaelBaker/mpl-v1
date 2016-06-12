module Mpl.TypeInterpreter where

import Mpl.Core   (Core(..), Type, Term)
import Data.Text  (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

type Env = Map.Map Text (Core Type)

interpret core = eval emptyEnv core

eval :: Env -> Core Term -> Core Term
eval env (CPolyApp (CPolyFunc param body) argType) = eval (bind param (typeEval env argType) env) body
eval env (CLam param tyParam body) = CLam param (typeEval env tyParam) (eval env body)
eval env (CTyAnn _ body) = eval env body
eval _ a = a

typeEval :: Env -> Core Type -> Core Type
typeEval _   CIntTy       = CIntTy
typeEval env (CTyParam a) = fromMaybe (error $ "Unbound type variable: " ++ show a) (lookupTyParam a env)
typeEval env (CTyOpApp (CTyOp param body) arg) = typeEval (bind param (typeEval env arg) env) body
typeEval env (CLamTy a b) = CLamTy (typeEval env a) (typeEval env b)
typeEval _ a = error $ "Invalid type to eval: " ++ show a

emptyEnv      = Map.empty  :: Env
bind          = Map.insert :: Text -> Core Type -> Env -> Env
lookupTyParam = Map.lookup :: Text -> Env -> Maybe (Core Type)
