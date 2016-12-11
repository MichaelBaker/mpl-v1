module Mpl.Untyped.BackendJS where

import Data.Functor.Foldable      (Base, cata)
import Language.JavaScript.Parser
import Mpl.ParsingUtils           (Parsed)
import Mpl.Untyped.Syntax         (SyntaxF(..))

import qualified Mpl.Common.BackendJS as CBE

translateToJS syntax = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression syntax

translateToJSExpression :: Parsed SyntaxF -> JSExpression
translateToJSExpression = cata translate

translate (Common common) = CBE.translate common
