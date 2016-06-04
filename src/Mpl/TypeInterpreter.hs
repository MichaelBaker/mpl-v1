module Mpl.TypeInterpreter where

import Data.Text (Text)
import Mpl.AST   (Core(..), CoreType(CUnknownTy), transform, typeOf, nameOf)
import qualified Data.Map.Strict as Map

type Env = Map.Map Text CoreType

interpret core = transform (apply Map.empty) core

apply ctx _ (CApp _ (CTyFunc _ _ param body) tyArg) = transform (apply $ binding param tyArg ctx) body
apply ctx _ (CFunc m e (p, t) body) = CFunc m e (p, typeName t ctx) $ transform (apply ctx) body
apply _ f a = f a

binding param (CIdent _ typeName) ctx = case typeOf typeName of
                                          Nothing -> Map.insert param CUnknownTy ctx
                                          Just a  -> Map.insert param a ctx
binding _ a _ = error $ "Invalid type identifier: " ++ show a

typeName name ctx = case Map.lookup name ctx of
                      Nothing -> nameOf CUnknownTy
                      Just a  -> nameOf a

