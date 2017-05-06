module Mpl.Untyped.BackendJS where

import           Language.JavaScript.Parser
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Untyped.Core
import           Mpl.Utils
import qualified Mpl.Common.BackendJS       as CBE
import qualified Mpl.Common.Core            as CC
import qualified Mpl.JSUtils                as JSU

translateToJS core = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression core

translateToJSExpression :: SourceAnnotated (CoreF (SourceAnnotated CC.Binder)) -> JSExpression
translateToJSExpression core =
  JSU.translateToJSExpression
    JSU.defaultJSState
    (cata translate core)

translate (Common common) = CBE.translate CBE.translateBinder common
