module Mpl.Typing where

import Mpl.Core  (Core(..), Type(..), metaOf)
import Data.Text (Text)
import Data.Map.Strict as Map

data TypeError = TypeError deriving (Show)
type Context = Map.Map Text Type

toTypedCore :: Core a -> (Maybe TypeError, Core Type)
toTypedCore core = let typedCore = typeCheck Map.empty core
                       in case metaOf typedCore of
                            TUnknown -> (Just TypeError, typedCore)
                            _        -> (Nothing, typedCore)

typeCheck :: Context -> Core a -> Core Type
typeCheck _ a@(CInt path _ val) = CInt path TInt val

typeCheck context a@(CIdent path _ name) =
  case Map.lookup name context of
    Nothing -> CIdent path TUnknown name
    Just ty -> CIdent path ty name

typeCheck context (CThunk path _ body) =
  let newBody = typeCheck context body
      in CThunk path (TThunk (metaOf newBody)) newBody

typeCheck context (CForce path _ f) =
  let newF = typeCheck context f
      in case metaOf newF of
           (TThunk _) -> CForce path (metaOf newF) newF
           _          -> CForce path TUnknown newF

typeCheck context (CFunc path _ param body) =
  let newBody = typeCheck context body
      in CFunc path (TFunc TUnknown (metaOf newBody)) param newBody

typeCheck context (CApp path _ f arg) =
  let newArg = typeCheck context arg
      newF   = typeCheck context f
      in case metaOf newF of
           TFunc paramType resultType ->
             if paramType == (metaOf newArg)
               then CApp path resultType newF newArg
               else CApp path TUnknown newF newArg
           _ -> CApp path TUnknown newF newArg

binding context param ty = Map.insert param ty context
