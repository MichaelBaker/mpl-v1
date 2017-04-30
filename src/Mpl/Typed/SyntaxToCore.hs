module Mpl.Typed.SyntaxToCore where

import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Core         as CC
import qualified Mpl.Common.Syntax       as CS
import qualified Mpl.Common.SyntaxToCore as CSTC
import qualified Mpl.Typed.Core          as C
import qualified Mpl.Typed.Syntax        as S

runTransform :: Eff (CSTC.EmbedCore (SourceAnnotated C.Binder) C.CoreF ': r) w -> Eff r w
runTransform =
  handleRelay
    pure
    (\(CSTC.EmbedCore core) q -> q (C.Common core))

runTransformBinder :: Eff (CSTC.TransformBinder S.Binder C.Binder ': r) w -> Eff r w
runTransformBinder =
  handleRelay
    pure
    (\CSTC.TransformBinder q -> q transformBinder)

transform annotation (S.Common common) = do
  CSTC.transform annotation common

transform annotation (S.TypeAnnotation r t) = do
  let type_ = convertType t
  expression <- r
  return (annotation :< C.TypeAnnotation expression type_)

convertType (S.TypeSymbol text) =
  C.TypeSymbol text

transformBinder span (S.CommonBinder (CS.Binder text)) =
  span :< C.CommonBinder (CC.Binder text)

transformBinder span (S.AnnotatedBinder r t) =
  span :< C.AnnotatedBinder r (convertType t)
