module Mpl.TypeInterpreter where

import Data.Text (Text)
import Mpl.Core  (Core(..), CoreType(..), transform)
import qualified Data.Map.Strict as Map

type Env = Map.Map Text CoreType

interpretTypes core = transform (apply Map.empty) core

apply ctx _ (CTyApp _ (CTyFunc _ _ p b) arg) = applyTyFun ctx p b arg
apply ctx _ a@(CFOmega _ _)                  = error $ "Cannot use type operators at the term level: " ++ show a
apply ctx _ (CFunc m e (p, t) body)          = CFunc m e (p, applyTy t ctx) $ transform (apply ctx) body
apply _ f a = f a

applyTyFun ctx p b tyArg = transform (apply $ binding ctx p $ applyOmega ctx tyArg) b

applyOmega ctx (CTSym name) = case Map.lookup name ctx of
                                Nothing -> error $ "Unbound type variable: " ++ show name
                                Just ty -> ty
applyOmega ctx (CTApp f a) = case applyOmega ctx f of
                               (CTFOmega p b) -> applyOmega (binding ctx p (applyOmega ctx a)) b
                               a              -> error $ "Tried to apply something that isn't a type operator: " ++ show f
applyOmega _ a = a

binding ctx param ty = Map.insert param ty ctx

applyTy (CTSym name) ctx = case Map.lookup name ctx of
                              Just ty -> ty
                              Nothing -> CUnknownTy
applyTy a _ = a
