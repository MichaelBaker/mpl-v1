module Mpl.Untyped.BackendJS where

import Data.Functor.Foldable      (Base, cata)
import Language.JavaScript.Parser
import Mpl.ParserUtils            (SourceAnnotated)
import Mpl.Untyped.Core           (CoreF(..))
import qualified Mpl.JSUtils          as JSU
import qualified Mpl.Common.BackendJS as CBE
import qualified Mpl.Common.Core      as CC

translateToJS core = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression core

translateToJSExpression
  :: SourceAnnotated (CoreF (SourceAnnotated CC.Binder))
  -> JSExpression
translateToJSExpression core =
  JSU.translateToJSExpression
    JSU.defaultJSState
    (cata translate core)

translate (Common common) = CBE.translate CBE.translateBinder common
