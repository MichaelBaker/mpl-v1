module Mpl.Untyped.BackendJS where

import Data.Functor.Foldable      (Base, cata)
import Language.JavaScript.Parser
import Mpl.ParserUtils            (SourceAnnotated)
import Mpl.Untyped.Syntax         (SyntaxF(..))
import qualified Mpl.JSUtils          as JSU
import qualified Mpl.Common.BackendJS as CBE
import qualified Mpl.Common.Syntax    as CS

translateToJS syntax = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression syntax

translateToJSExpression :: SourceAnnotated (SyntaxF (SourceAnnotated CS.Binder)) -> JSExpression
translateToJSExpression syntax =
  JSU.translateToJSExpression
    JSU.defaultJSState
    (cata translate syntax)

translate (Common common) = CBE.translate CBE.translateBinder common
