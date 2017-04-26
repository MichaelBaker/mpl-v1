module Mpl.Typed.Typecheck where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Map.Strict            as Map
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Syntax          as CS
import qualified Mpl.Typed.Syntax           as TS

--------------------------------------------------------------------------------
-- Datatypes for typechecking

data Type
  = IntegerType
  deriving (Show, Eq)

data Context
  = Context
  { symbolTypes     :: Map.Map Text Type
  , typeSymbolTypes :: Map.Map Text Type
  }
  deriving (Show)

standardContext =
  Context
  { symbolTypes     = Map.empty
  , typeSymbolTypes = typeSymbolTable
  }
  where typeSymbolTable =
          Map.empty
          |> Map.insert "Integer" IntegerType

data TypeError
  = UnimplementedError Text
  | UnboundTypeSymbol  Text
  | CannotInferSymbol  Text
  | IncompatibleType   Text
  deriving (Show)

type Typechecker a = StateT Context (Except TypeError) a

eval :: Typechecker Type -> Context -> Either TypeError Type
eval action state = runExcept $ evalStateT action state

typecheck expression = eval (infer expression) standardContext

--------------------------------------------------------------------------------
-- Inference (type synthesis) mode

infer (_ :< TS.Common common) =
  inferCommon common

infer (_ :< a@(TS.TypeAnnotation expression ty)) = do
  ty'    <- getType ty
  isType <- check expression ty'
  if isType
    then
      return ty'
    else
      throwError $ IncompatibleType $ showText a

inferCommon (CS.Literal literal) =
  inferLiteral literal

inferCommon (CS.Symbol symbol) = do
  maybeTy <- lookupSymbolType symbol
  case maybeTy of
    Nothing ->
      throwError $ CannotInferSymbol symbol
    Just ty ->
      return ty

inferCommon a =
  throwError $ UnimplementedError $ showText a

inferLiteral (CS.IntegerLiteral _) =
  return IntegerType

--------------------------------------------------------------------------------
-- Checking mode

check expression ty = do
  inferredType <- infer expression
  return $ isSubtype inferredType ty

--------------------------------------------------------------------------------
-- Subtyping

isSubtype IntegerType IntegerType = True

--------------------------------------------------------------------------------
-- Helpers

addSymbol text ty =
  modify' $ \state ->
    state { symbolTypes = Map.insert text ty (symbolTypes state) }

lookupSymbolType symbol = do
  table <- gets symbolTypes
  return $ Map.lookup symbol table

getType (TS.TypeSymbol symbol) = do
  table <- gets typeSymbolTypes
  case Map.lookup symbol table of
    Just ty -> return ty
    Nothing -> throwError $ UnboundTypeSymbol $ showText symbol
