module Mpl.Typed.Typecheck where

import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Mpl.Parser.SourceSpan
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Typed.Core
import           Mpl.Utils
import qualified Data.List                  as List
import qualified Mpl.Common.Core            as CC

type SourceCore =
  SourceAnnotated (CoreF SourceType SourceBinder)

type SourceBinder =
  SourceAnnotated (Binder SourceType)

type SourceType =
  SourceAnnotated Type

type InferenceType =
  Annotated Type (SourceSpan, InferenceReason)

deriving instance Data InferenceType

type CommonCore =
  CC.CoreF SourceBinder SourceCore

--------------------------------------------------------------------------------
-- Datatypes for typechecking

data Context
  = Context
  { symbolTypes     :: [(Text, InferenceType)]
  , typeSymbolTypes :: [(Text, InferenceType)]
  }
  deriving (Show)

standardContext =
  Context
  { symbolTypes =
      []
  , typeSymbolTypes =
      [ ("Integer", ((emptySpan, NoReason) :< IntegerType))
      , ("UTF8",    ((emptySpan, NoReason) :< UTF8StringType))
      ]
  }

data TypeError
  = Unimplemented
      SourceSpan       -- ^ Unimplemented feature
      Text             -- ^ Explanation
  | UnboundTypeSymbol
      SourceSpan       -- ^ Annotation of type symbol
      Text             -- ^ The type symbol
  | CannotInferSymbol
      SourceSpan       -- ^ Annotation of symbol
      Text             -- ^ The symbol
  | ApplicationOfNonFunction
      SourceSpan       -- ^ Annotation of function
      InferenceType    -- ^ The type that was inferred
  | InvalidArgument
      SourceSpan       -- ^ The function code
      InferenceType    -- ^ The param type
      SourceSpan       -- ^ The argument code
      InferenceType    -- ^ The argument type
  | InvalidTypeAnnotation
      InferenceType    -- ^ The inferred type
      SourceSpan       -- ^ The expression the type was inferred for
      InferenceType    -- ^ The annotated type
      SourceSpan       -- ^ The code of the annotation
  deriving (Show, Eq, Typeable, Data)

data InferenceReason
  = NoReason
  | InferredFromSyntax
  | InferredSymbolType
      SourceSpan      -- ^ The symbol
      InferenceReason -- ^ The reason for the underlying type
  | InferredFromTypeAnnotation
      SourceSpan      -- ^ The binder with annotation
  | InferredIntegerLiteral
      SourceSpan      -- ^ The code of the integer
  | InferredUTF8StringLiteral
      SourceSpan      -- ^ The code of the string
  | InferredApplication
      SourceSpan      -- ^ The code of the function being applied
      InferenceType   -- ^ The type of the function
      SourceSpan      -- ^ The code of the argument
      InferenceType   -- ^ The type of the argument
      InferenceType   -- ^ The type of the body
  deriving (Show, Eq, Typeable, Data)

type Typechecker effects =
  ( Member (State Context) effects
  , Member (Exc (TypeError, Context)) effects
  )

runTypecheck :: Eff '[State Context, Exc (TypeError, Context)] InferenceType
             -> Context
             -> Either (TypeError, Context) (InferenceType, Context)
runTypecheck action state =
  runState action state
    |> runError
    |> run

--------------------------------------------------------------------------------
-- Inference (type synthesis) mode

infer :: (Typechecker effects) => SourceCore -> Eff effects InferenceType

infer (annotation :< Common common) =
  inferCommon annotation common

infer (_ :< a@(TypeAnnotation expression ty)) = do
  ty' <- getType ty
  (inferredType, isType) <- check expression ty'
  if isType
    then
      return ty'
    else do
      context <- getContext
      throwError (InvalidTypeAnnotation inferredType (annotation expression) ty' (annotation ty), context)

inferCommon :: (Typechecker effects) => SourceSpan -> CommonCore -> Eff effects InferenceType

inferCommon annotation (CC.Symbol symbol) = do
  maybeTy <- lookupSymbolType symbol
  case maybeTy of
    Nothing -> do
      context <- getContext
      throwError (CannotInferSymbol annotation symbol, context)
    Just type_ -> do
      let reason = InferredSymbolType annotation (getReason type_)
      return (setReason reason type_)

inferCommon annotation (CC.Literal literal) = do
  inferLiteral annotation literal

inferCommon annotation (CC.Function binder body) =
  case binder of
    (span :< AnnotatedBinder (_ :< CommonBinder (CC.Binder name)) type_) -> do
      inferredParamType <- getType type_
      let paramType = setReason (InferredFromTypeAnnotation span) inferredParamType
      pushSymbol name paramType
      bodyType <- infer body
      popSymbol
      return ((annotation, NoReason) :< FunctionType paramType bodyType)
    (span :< _) -> do
      context <- getContext
      throwError (Unimplemented span "Cannot infer the types of functions with unannotated parameters.", context)

inferCommon _ (CC.Application function argument) = do
  functionType <- infer function
  argumentType <- infer argument

  case functionType of
    (_ :< FunctionType paramType bodyType) -> do
      if isSubtype argumentType paramType
        then
          let reason = InferredApplication (annotation function) functionType (annotation argument) argumentType bodyType
          in  return ((getSpan bodyType, reason) :< project bodyType)
        else do
          context <- getContext
          throwError (InvalidArgument (annotation function) paramType (annotation argument) argumentType, context)
    _ -> do
      context <- getContext
      throwError (ApplicationOfNonFunction (annotation function) functionType, context)

inferLiteral annotation (CC.IntegerLiteral _) =
  return ((annotation, InferredIntegerLiteral annotation) :< IntegerType)

inferLiteral annotation (CC.UTF8StringLiteral _) =
  return ((annotation, InferredUTF8StringLiteral annotation) :< UTF8StringType)

--------------------------------------------------------------------------------
-- Checking mode

check :: (Typechecker effects) => SourceCore -> InferenceType -> Eff effects (InferenceType, Bool)
check expression ty = do
  inferredType <- infer expression
  return (inferredType, isSubtype inferredType ty)

--------------------------------------------------------------------------------
-- Subtyping

isSubtype (_ :< IntegerType) (_ :< IntegerType) =
  True

isSubtype (_ :< UTF8StringType) (_ :< UTF8StringType) =
  True

isSubtype _ _ =
  False

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

getType (annotation :< TypeSymbol symbol) = do
  context <- get
  case associativeLookup symbol (typeSymbolTypes context) of
    Just ty ->
      return ty
    Nothing -> do
      let error = UnboundTypeSymbol annotation symbol
      throwError (error, context)

getType type_ = do
  return $ envcata (\span ty -> ((span, InferredFromSyntax) :< ty)) type_

getContext :: (Typechecker effects) => Eff effects Context
getContext = get

getSpan =
  fst . annotation

getReason =
  snd . annotation

setReason reason ((span, _) :< type_) =
  ((span, reason) :< type_)
