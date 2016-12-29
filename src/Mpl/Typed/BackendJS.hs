module Mpl.Typed.BackendJS where

import Language.JavaScript.Parser

import Data.Text               (unpack)
import Data.Functor.Foldable   (Base, cata)
import Mpl.Typed.Syntax        (SyntaxF(..), Type(..))
import Mpl.ParserUtils         (Parsed)
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
translate (TypeAnnotation expression annotation) = do
  expression' <- expression
  return $ CBE.addComment expression' (typeComment annotation)

typeComment (TypeSymbol text) = unpack text
