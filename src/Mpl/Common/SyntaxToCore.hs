module Mpl.Common.SyntaxToCore where

import           Mpl.Parser.SourceSpan
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Core     as C
import qualified Mpl.Common.Syntax   as S

-- | This effect is used when the common syntax -> core transformation generates a new common Core node that needs to be embedded in the host language's core.
data EmbedCore binder recurse result where
  EmbedCore :: C.CoreF binder (SourceAnnotated (recurse binder))
            -> EmbedCore binder recurse (recurse binder (SourceAnnotated (recurse binder)))

effEmbedCore :: Member (EmbedCore binder recurse) effects
             => C.CoreF binder (SourceAnnotated (recurse binder))
             -> Eff effects (recurse binder (SourceAnnotated (recurse binder)))
effEmbedCore = send . EmbedCore

-- | This effect is used when a binder syntax needs to be converted to core.
--
-- This is an effect because the binder type is parameterized in Common. As a result, the conversion function must be chosen separately for each type of binder.
data TransformBinder syntaxBinder coreBinder result where
  TransformBinder :: TransformBinder
                       syntaxBinder
                       coreBinder
                       (SourceSpan -> syntaxBinder (SourceAnnotated coreBinder) -> SourceAnnotated coreBinder)

effTransformBinder :: Member (TransformBinder syntaxBinder coreBinder) effects
                   => Eff effects (SourceSpan -> syntaxBinder (SourceAnnotated coreBinder) -> SourceAnnotated coreBinder)
effTransformBinder = send TransformBinder

runTransform :: Eff (EmbedCore (SourceAnnotated C.Binder) C.CoreF ': r) w -> Eff r w
runTransform =
  handleRelay
    pure
    (\(EmbedCore core) q -> q core)

runTransformBinder :: Eff (TransformBinder S.Binder C.Binder ': r) w -> Eff r w
runTransformBinder =
  handleRelay
    pure
    (\TransformBinder q -> q transformBinder)

transform annotation (S.Literal literal) = do
  core <- effEmbedCore (C.Literal $ convertLiteral literal)
  return (annotation :< core)

transform annotation (S.Symbol text) = do
  core <- effEmbedCore (C.Symbol text)
  return (annotation :< core)

transform annotation (S.Function binders body) = do
  (_ :< curriedFunction) <- curryFunction annotation binders body
  return (annotation :< curriedFunction)

transform annotation (S.Application fun args) = do
  newFun <- fun
  (_ :< curriedApplication) <- curryApplication annotation args newFun
  return (annotation :< curriedApplication)

convertLiteral (S.IntegerLiteral integer) =
  C.IntegerLiteral integer

curryFunction _ [] _ =
  error "Cannot have a function with no parameters" -- TODO: Fold this into effectful error handling

curryFunction originalAnnotation (p:[]) body = do
  newBody         <- body
  transformBinder <- effTransformBinder
  core            <- effEmbedCore $ C.Function (envcata transformBinder p) newBody
  let newSpan = spanUnion (annotation p) originalAnnotation
  return (newSpan :< core)

curryFunction originalAnnotation (p:ps) body = do
  newBody         <- curryFunction originalAnnotation ps body
  transformBinder <- effTransformBinder
  newFunction     <- effEmbedCore $ C.Function (envcata transformBinder p) newBody
  let newSpan = spanUnion (annotation p) originalAnnotation
  return (newSpan :< newFunction)

curryApplication _ [] _ =
  error "Cannot have an application with no arguments" -- TODO: Fold this into effectful error handling

curryApplication originalAnnotation (a:[]) fun = do
  newArg <- a
  core   <- effEmbedCore $ C.Application fun newArg
  let newSpan = spanUnion originalAnnotation (annotation newArg)
  return (newSpan :< core)

curryApplication originalAnnotation (a:as) fun = do
  newArg         <- a
  newApplication <- effEmbedCore $ C.Application fun newArg
  let newSpan = spanUnion originalAnnotation (annotation newArg)
  curryApplication originalAnnotation as (newSpan :< newApplication)

transformBinder span (S.Binder text) =
  span :< C.Binder text
