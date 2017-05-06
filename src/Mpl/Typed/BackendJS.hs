module Mpl.Typed.BackendJS where

import           Language.JavaScript.Parser
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Typed.Core
import           Mpl.Utils
import qualified Mpl.Common.BackendJS       as CBE
import qualified Mpl.Common.Core            as CC
import qualified Mpl.JSUtils                as JSU

translateToJS syntax = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression syntax

translateToJSExpression :: SourceAnnotated (CoreF (SourceAnnotated Binder)) -> JSExpression
translateToJSExpression syntax =
  JSU.translateToJSExpression
    JSU.defaultJSState
    (cata translate syntax)

translate (Common common) =
  CBE.translate translateBinder common

translate (TypeAnnotation expression annotation) = do
  expression' <- expression
  return $ CBE.addComment expression' (typeComment annotation)

translateBinder (CommonBinder binder) =
  CBE.translateBinder binder

translateBinder (AnnotatedBinder recursiveBinder type_) = do
  binder <- recursiveBinder
  return $ CBE.addComment binder (typeComment type_)

typeComment (TypeSymbol text) = textToString text
