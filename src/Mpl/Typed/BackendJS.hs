module Mpl.Typed.BackendJS where

import Language.JavaScript.Parser

import Data.Text               (unpack)
import Data.Functor.Foldable   (Base, cata)
import Mpl.Typed.Syntax        (SyntaxF(..), Type(..))
import Mpl.ParserUtils         (Parsed)

import qualified Mpl.Common.BackendJS as CBE

translateToJS syntax = JSAstExpression expression JSNoAnnot
  where expression = translateToJSExpression syntax

translateToJSExpression :: Parsed SyntaxF -> JSExpression
translateToJSExpression = cata translate

translate (Common common) = CBE.translate common
translate (TypeAnnotation expression annotation) = CBE.addComment expression (typeComment annotation)

typeComment (TypeSymbol text) = unpack text
