module Mpl.Typed.BackendJS where

import           Language.JavaScript.Parser
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Typed.Core
import           Mpl.Utils
import qualified Data.Text                  as Text
import qualified Mpl.Common.BackendJS       as CBE
import qualified Mpl.Common.Core            as CC
import qualified Mpl.JSUtils                as JSU

type SourceCore =
  SourceAnnotated (CoreF SourceType SourceBinder)

type SourceBinder =
  SourceAnnotated (Binder SourceType)

type SourceType =
  SourceAnnotated Type

translateToJS syntax = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression syntax

translateToJSExpression :: SourceCore -> JSExpression
translateToJSExpression syntax =
  JSU.translateToJSExpression
    JSU.defaultJSState
    (cata translate syntax)

translate (Common common) =
  CBE.translate translateBinder common

translate (TypeAnnotation expression annotation) = do
  expression' <- expression
  return $ CBE.addComment expression' (textToString $ envcata typeComment annotation)

translateBinder (CommonBinder binder) =
  CBE.translateBinder binder

translateBinder (AnnotatedBinder recursiveBinder type_) = do
  binder <- recursiveBinder
  return $ CBE.addComment binder (textToString $ envcata typeComment type_)

typeComment _ (TypeSymbol text) =
  text

typeComment _ (IntegerType) =
  "Integer"

typeComment _ (UTF8StringType) =
  "UTF8"

typeComment _ (FunctionType type0 type1) =
  let function = Text.intercalate " " ["->", type0, type1]
  in Text.concat ["(", function, ")"]

typeComment _ (TypeApplication type0 type1) =
  let application = Text.intercalate " " [type0, type1]
  in Text.concat ["(", application, ")"]
