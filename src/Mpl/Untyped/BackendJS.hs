module Mpl.Untyped.BackendJS where

import Data.Functor.Foldable      (Base, cata)
import Language.JavaScript.Parser
import Mpl.ParserUtils            (Parsed)
import Mpl.Untyped.Syntax         (SyntaxF(..))
import qualified Mpl.JSUtils          as JSU
import qualified Mpl.Common.BackendJS as CBE

translateToJS syntax = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression syntax

translateToJSExpression :: Parsed SyntaxF -> JSExpression
translateToJSExpression syntax =
  JSU.translateToJSExpression
    JSU.defaultJSState
    (cata translate syntax)

translate (Common common) = CBE.translate common
