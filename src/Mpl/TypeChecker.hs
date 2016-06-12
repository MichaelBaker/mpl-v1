module Mpl.TypeChecker where

import Mpl.Core   (Core(..), Type, Term)
import Data.Maybe (fromMaybe)
import Data.Text  (Text)
import qualified Data.Map.Strict as Map

data TypeError = Contradiction (Core Type) (Core Type) (Core Term)
  deriving (Show, Eq)

type Env = Map.Map Text (Core Type)

typeContradictions = check Map.empty

check :: Env -> Core Term -> [TypeError]
check env a@(CTermApp (CLam param paramTy body) arg) = let argTy = typeOf env arg in
                                                           if paramTy == argTy
                                                             then check (bind param paramTy env) body
                                                             else (Contradiction paramTy argTy a) : check (bind param paramTy env) body
check _ (CSym _) = []
-- TODO: Remove this
check _ a = error $ "check not implemented for: " ++ show a

typeOf :: Env -> Core Term -> Core Type
typeOf _   (CInt _)  = CIntTy
typeOf _   (CReal _) = CRealTy
typeOf _   (CText _) = CTextTy
typeOf env (CSym a)  = fromMaybe CUnknownTy (lookupParam a env)
-- TODO: Remove this
typeOf _ a = error $ "typeOf not yet implemented for: " ++ show a

emptyEnv    = Map.empty  :: Env
bind        = Map.insert :: Text -> Core Type -> Env -> Env
lookupParam = Map.lookup :: Text -> Env -> Maybe (Core Type)
