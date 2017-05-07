module Mpl.Typed.Typecheck where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Map.Strict            as Map
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import qualified Data.List                  as List
import qualified Mpl.Common.Core            as CC
import qualified Mpl.Typed.Core             as TC

--------------------------------------------------------------------------------
-- Datatypes for typechecking

data Type
  = IntegerType
  | FunctionType Type Type
  deriving (Show, Eq)

data Context
  = Context
  { symbolTypes     :: [(Text, Type)]
  , typeSymbolTypes :: Map.Map Text Type
  }
  deriving (Show)

standardContext =
  Context
  { symbolTypes     = []
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

infer :: SourceAnnotated (TC.CoreF (SourceAnnotated TC.Binder)) -> Typechecker Type
infer (_ :< TC.Common common) =
  inferCommon common

infer (_ :< a@(TC.TypeAnnotation expression ty)) = do
  ty'    <- getType ty
  isType <- check expression ty'
  if isType
    then
      return ty'
    else
      throwError $ IncompatibleType $ showText a

inferCommon (CC.Literal literal) =
  inferLiteral literal

inferCommon (CC.Symbol symbol) = do
  maybeTy <- lookupSymbolType symbol
  case maybeTy of
    Nothing ->
      throwError $ CannotInferSymbol symbol
    Just ty ->
      return ty

inferCommon (CC.Function binder body) =
  case binder of
    _ :< TC.AnnotatedBinder (_ :< TC.CommonBinder (CC.Binder name)) type_ -> do
      paramType <- getType type_
      pushSymbol name paramType
      bodyType <- infer body
      popSymbol
      return $ FunctionType paramType bodyType
    _ ->
      throwError $ UnimplementedError "Cannot infer the types of functions with unannotated parameters"

inferCommon a =
  throwError $ UnimplementedError $ showText a

inferLiteral (CC.IntegerLiteral _) =
  return IntegerType

--------------------------------------------------------------------------------
-- Checking mode

check expression ty = do
  inferredType <- infer expression
  return $ isSubtype inferredType ty

--------------------------------------------------------------------------------
-- Subtyping

isSubtype IntegerType IntegerType = True
isSubtype _ _                     = False

--------------------------------------------------------------------------------
-- Helpers

pushSymbol text ty =
  modify' $ \state ->
    state { symbolTypes = (text, ty) : (symbolTypes state) }

popSymbol =
  modify' $ \state ->
    state { symbolTypes = List.drop 1 (symbolTypes state) }

lookupSymbolType symbol = do
  table <- gets symbolTypes
  List.find ((== symbol) . fst) table
    |> fmap snd
    |> return

getType (TC.TypeSymbol symbol) = do
  table <- gets typeSymbolTypes
  case Map.lookup symbol table of
    Just ty -> return ty
    Nothing -> throwError $ UnboundTypeSymbol $ showText symbol
