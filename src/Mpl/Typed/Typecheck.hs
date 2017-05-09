module Mpl.Typed.Typecheck where

import           Data.Map.Strict            as Map
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Exception
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
  = UnimplementedError
      Text
  | UnboundTypeSymbol
      SourceSpan       -- ^ Annotation of type symbol
      Text             -- ^ The type symbol
  | CannotInferSymbol
      Text
  | ApplicationOfNonFunction
      SourceSpan       -- ^ Annotation of type symbol
      Text             -- ^ The type that was inferred
  | InvalidArgument
      Text
  | InvalidTypeAnnotation
      Text
  deriving (Show, Eq, Typeable, Data)

type Typechecker effects =
  ( Member (State Context) effects
  , Member (Exc (TypeError, Context)) effects
  )

type Core =
  SourceAnnotated (TC.CoreF (SourceAnnotated TC.Binder))

type CommonCore =
  CC.CoreF (SourceAnnotated TC.Binder) (Core)


runTypecheck :: Eff '[State Context, Exc (TypeError, Context)] Type
             -> Context
             -> Either (TypeError, Context) (Type, Context)
runTypecheck action state =
  runState action state
    |> runError
    |> run

--------------------------------------------------------------------------------
-- Inference (type synthesis) mode

infer :: (Typechecker effects) => Core -> Eff effects Type

infer (annotation :< TC.Common common) =
  inferCommon annotation common

infer (annotation :< a@(TC.TypeAnnotation expression ty)) = do
  ty'    <- getType annotation ty
  isType <- check expression ty'
  if isType
    then
      return ty'
    else do
      context <- getContext
      throwError (InvalidTypeAnnotation $ showText a, context)

inferCommon :: (Typechecker effects) => SourceSpan -> CommonCore -> Eff effects Type

inferCommon _ (CC.Literal literal) =
  inferLiteral literal

inferCommon _ (CC.Symbol symbol) = do
  maybeTy <- lookupSymbolType symbol
  case maybeTy of
    Nothing -> do
      context <- getContext
      throwError (CannotInferSymbol symbol, context)
    Just ty ->
      return ty

inferCommon annotation (CC.Function binder body) =
  case binder of
    _ :< TC.AnnotatedBinder (_ :< TC.CommonBinder (CC.Binder name)) type_ -> do
      paramType <- getType annotation type_
      pushSymbol name paramType
      bodyType <- infer body
      popSymbol
      return $ FunctionType paramType bodyType
    _ -> do
      context <- getContext
      throwError (UnimplementedError "Cannot infer the types of functions with unannotated parameters", context)

inferCommon _ (CC.Application function argument) = do
  functionType <- infer function
  argumentType <- infer argument

  case functionType of
    FunctionType paramType bodyType -> do
      if isSubtype argumentType paramType
        then
          return bodyType
        else do
          context <- getContext
          throwError (InvalidArgument $ showText argumentType, context)
    a -> do
      context <- getContext
      throwError (ApplicationOfNonFunction (annotation function) (showText functionType), context)

inferLiteral (CC.IntegerLiteral _) =
  return IntegerType

--------------------------------------------------------------------------------
-- Checking mode

check :: (Typechecker effects) => Core -> Type -> Eff effects Bool
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
  modify $ \state ->
    state { symbolTypes = (text, ty) : (symbolTypes state) }

popSymbol :: (Typechecker effects) => Eff effects ()
popSymbol =
  modify $ \state ->
    state { symbolTypes = List.drop 1 (symbolTypes state) }

lookupSymbolType symbol = do
  context <- get
  List.find ((== symbol) . fst) (symbolTypes context)
    |> fmap snd
    |> return

getType annotation (TC.TypeSymbol symbol) = do
  context <- get
  case Map.lookup symbol (typeSymbolTypes context) of
    Just ty ->
      return ty
    Nothing -> do
      let error = UnboundTypeSymbol annotation symbol
      throwError (error, context)

getContext :: (Typechecker effects) => Eff effects Context
getContext = get
