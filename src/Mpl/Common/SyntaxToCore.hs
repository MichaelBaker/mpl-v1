module Mpl.Common.SyntaxToCore where

import           Mpl.Prelude
import           Mpl.Utils
import qualified Control.Monad.Freer as F
import qualified Mpl.Common.Core     as C
import qualified Mpl.Common.Syntax   as S

-- | This effect is used when the common syntax -> core transformation generates a new common Core node that needs to be embedded in the host language's core.
data EmbedCore recurse result where
  EmbedCore :: C.CoreF C.SourceBinder recurse -> EmbedCore recurse (Base recurse recurse)

embedCore :: F.Member (EmbedCore recurse) effects => C.CoreF C.SourceBinder recurse -> F.Eff effects (Base recurse recurse)
embedCore = F.send . EmbedCore

transform annotation (S.Symbol text) = do
  core <- embedCore (C.Symbol text)
  return (annotation :< core)

transform _ a = error $ "SyntaxToCore.transform not implemented for " ++ show a
