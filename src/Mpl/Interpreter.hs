module Mpl.Interpreter where

import Mpl.Core   (Core(..), Term)
import Data.Maybe (fromMaybe)
import Data.Text  (Text)
import qualified Data.Map.Strict as Map

type Env = Map.Map Text (Core Term)

eval :: Core Term -> Core Term
eval core = eval' emptyEnv core

eval' :: Env -> Core Term -> Core Term
eval' env (CTermApp (CLam param _ body) a) = eval' (bind param (eval' env a) env) body
eval' env (CSym a) = fromMaybe (error $ "Unbound parameter: " ++ show a) (lookupParam a env)
eval' _ a = a

emptyEnv    = Map.empty  :: Env
bind        = Map.insert :: Text -> Core Term -> Env -> Env
lookupParam = Map.lookup :: Text -> Env -> Maybe (Core Term)
