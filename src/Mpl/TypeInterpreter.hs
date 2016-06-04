module Mpl.TypeInterpreter where

import Data.Text (Text)
import Data.List (find)
import Mpl.AST   (Core(..), CoreType(..), transform)
import qualified Data.Map.Strict as Map

type Env = Map.Map Text CoreType

interpret core = transform (apply Map.empty) core

apply ctx _ (CApp _ (CTyFunc _ _ param body) tyArg) = transform (apply $ binding param tyArg ctx) body
apply ctx _ (CFunc m e (p, t) body) = CFunc m e (p, typeName t ctx) $ transform (apply ctx) body
apply _ f a = f a

binding param typeName ctx = Map.insert param (typeOf typeName) ctx

mapping = [
  ("unit", CUnitTy),
  ("int",  CIntTy),
  ("real", CRealTy),
  ("text", CTextTy),
  ("list", CListTy),
  ("map",  CMapTy)
  ]

typeName name ctx = case Map.lookup name ctx of
                      Nothing -> "unknown"
                      Just a  -> nameOf a

nameOf ty = case find ((== ty) . snd) mapping of
              Nothing -> error $ "No name for type: " ++ show ty
              Just a  -> fst a

typeOf (CIdent _ name) = case find ((== name) . fst) mapping of
                           Nothing -> error $ "Invalid type name: " ++ show name
                           Just a  -> snd a
typeOf a = error $ "Invalid type name: " ++ show a
