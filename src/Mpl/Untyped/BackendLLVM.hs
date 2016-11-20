module Mpl.Untyped.BackendLLVM where

import LLVM.General.AST

import Mpl.Untyped.Syntax    (Syntax, SyntaxF(..))
import Data.Functor.Foldable (Base, para)
import Mpl.Annotation        (Cofree((:<)), number)
import Data.Function         ((&))

import qualified Mpl.Common.BackendLLVM as CBE

translateToLLVM :: Syntax -> String
translateToLLVM syntax = syntax & number & para translate & show

translate (Common common) = CBE.translate common
