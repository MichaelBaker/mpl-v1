module Mpl.Typed.BackendJS where

import Data.Functor.Foldable
import Data.Text
import Language.JavaScript.Parser
import Mpl.ParserUtils
import Mpl.Prelude
import Mpl.Typed.Syntax
import qualified Mpl.Common.BackendJS as CBE
import qualified Mpl.Common.Syntax    as CS
import qualified Mpl.JSUtils          as JSU

translateToJS _ = error "Convert typed backendjs to operate on Core"
-- translateToJS syntax = JSAstExpression expression JSNoAnnot
--   where expression = translateToJSExpression syntax

-- translateToJSExpression :: SourceAnnotated (SyntaxF (SourceAnnotated Binder)) -> JSExpression
-- translateToJSExpression syntax =
--   JSU.translateToJSExpression
--     JSU.defaultJSState
--     (cata translate syntax)
-- 
-- translate (Common common) = CBE.translate translateBinder common
-- translate (TypeAnnotation expression annotation) = do
--   expression' <- expression
--   return $ CBE.addComment expression' (typeComment annotation)
-- 
-- translateBinder (CommonBinder binder) = CBE.translateBinder binder
-- translateBinder (AnnotatedBinder _ _) = error "translateBinder not implemented"
-- 
-- typeComment (TypeSymbol text) = unpack text
