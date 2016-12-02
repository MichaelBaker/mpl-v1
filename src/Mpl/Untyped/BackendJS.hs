module Mpl.Untyped.BackendJS where

import Language.JavaScript.Parser

import Mpl.Untyped.Syntax    (Syntax, SyntaxF(..))
import Data.Functor.Foldable (Base, cata)

import qualified Mpl.Common.BackendJS as CBE

translateToJS syntax = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression syntax

translateToJSExpression :: Syntax -> JSExpression
translateToJSExpression = cata translate

translate (Common common) = CBE.translate common