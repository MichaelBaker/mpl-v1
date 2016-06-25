module Mpl.Typing where

import Mpl.Core  (Core(..), Type(..), typeOf)
import Data.Text (Text)
import Data.Map.Strict as Map

data TypeError = TypeError deriving (Show)
type Context = Map.Map Text Type

toTypedCore :: Core -> (Maybe TypeError, Core)
toTypedCore core = case typeCheck Map.empty core of
                     Nothing     -> (Just TypeError, core)
                     Just result -> case typeOf result of
                                      TUnknown -> (Just TypeError, core)
                                      _        -> (Nothing, result)

typeCheck _ a@(CInt _ TInt _) = Just a
typeCheck _ a@(CInt _ _ _)    = Nothing
typeCheck context a@(CIdent path _ name) = do
  identType <- Map.lookup name context
  Just $ CIdent path identType name
typeCheck context (CThunk path _ body) = do
  newBody <- typeCheck context body
  Just $ CThunk path (TThunk (typeOf newBody)) newBody
typeCheck context (CForce path _ f) = do
  newF <- typeCheck context f
  case typeOf newF of
    (TThunk _) -> Just $ CForce path (typeOf newF) newF
    _          -> Nothing
typeCheck _ a@(CFunc _ _ _ _) = Just a
typeCheck context (CApp path _ f arg) = do
  newArg <- typeCheck context arg
  newF   <- typeCheckFunction context f (typeOf newArg)
  case typeOf newF of
    TFunc TUnknown _ -> Nothing
    TFunc _ result   -> Just $ CApp path result newF newArg
    _                -> Nothing

typeCheckFunction context (CFunc path _ param body) argType = do
  newBody <- typeCheck (binding context param argType) body
  Just $ CFunc path (TFunc argType (typeOf newBody)) param newBody
typeCheckFunction _ _ _ = Nothing

binding context param ty = Map.insert param ty context
