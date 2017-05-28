module Mpl.Typed.SyntaxToCore where

import           Mpl.Parser.SourceSpan
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Typed.Core
import           Mpl.Utils
import qualified Mpl.Common.Core         as CC
import qualified Mpl.Common.Syntax       as CS
import qualified Mpl.Common.SyntaxToCore as CSTC
import qualified Mpl.Typed.Syntax        as S

type SourceCore =
  SourceAnnotated (CoreF SourceType SourceBinder)

type SourceBinder =
  SourceAnnotated (Binder SourceType)

type SourceType =
  SourceAnnotated Type

type SyntaxType =
  SourceAnnotated S.Type

runTransform :: Eff (CSTC.EmbedCore SourceBinder (CoreF SourceType) ': r) w -> Eff r w
runTransform =
  handleRelay
    pure
    (\(CSTC.EmbedCore core) q -> q (Common core))

runTransformBinder :: Eff (CSTC.TransformBinder (S.Binder SyntaxType) (Binder SourceType) ': r) w -> Eff r w
runTransformBinder =
  handleRelay
    pure
    (\CSTC.TransformBinder q -> q transformBinder)

transform annotation (S.Common common) = do
  CSTC.transform annotation common

transform annotation (S.TypeAnnotation r t) = do
  let type_ = envcata convertType t
  expression <- r
  return (annotation :< TypeAnnotation expression type_)

transformBinder span (S.CommonBinder (CS.Binder text)) =
  span :< CommonBinder (CC.Binder text)

transformBinder span (S.AnnotatedBinder r t) =
  spanUnion (annotation r) (annotation t) :< AnnotatedBinder r (envcata convertType t)

convertType annotation (S.TypeSymbol text) =
  annotation :< TypeSymbol text

convertType annotation (S.TypeApplication function arguments) =
  curryTypeApplication annotation arguments function

curryTypeApplication _ [] _ =
  error "Cannot have a type application with no arguments" -- TODO: Fold this into effectful error handling

curryTypeApplication originalAnnotation (arg:[]) fun =
  let newSpan = spanUnion originalAnnotation (annotation arg)
  in (newSpan :< TypeApplication fun arg)

curryTypeApplication originalAnnotation (arg:as) fun =
  let newSpan = spanUnion originalAnnotation (annotation arg)
  in curryTypeApplication originalAnnotation as (newSpan :< TypeApplication fun arg)
