module Mpl.Untyped.BackendLLVM where

import LLVM.General.AST

import Mpl.Untyped.Syntax    (Syntax, SyntaxF(..))
import Data.Functor.Foldable (Base, cata)

import qualified Mpl.Common.BackendLLVM as CBE

translateToLLVM :: Syntax -> String
translateToLLVM _ = "wat"
