module Mpl.Untyped.SyntaxToCore where

import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Core         as CC
import qualified Mpl.Common.SyntaxToCore as CSTC
import qualified Mpl.Untyped.Core        as C
import qualified Mpl.Untyped.Syntax      as S

runTransform :: Eff (CSTC.EmbedCore (SourceAnnotated CC.Binder) C.CoreF ': r) w -> Eff r w
runTransform =
  handleRelay
    pure
    (\(CSTC.EmbedCore core) q -> q (C.Common core))

runTransformBinder = CSTC.runTransformBinder

transform annotation (S.Common common) = do
  CSTC.transform annotation common
