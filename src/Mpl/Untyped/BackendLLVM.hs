module Mpl.Untyped.BackendLLVM where

import LLVM.General.AST

import Mpl.Untyped.Syntax    (Syntax, SyntaxF(..))
import Data.Functor.Foldable (Base, cata)
import Mpl.Annotation        (Cofree((:<)), number)
import Data.Function         ((&))
import Control.Comonad       (extend)

import qualified Mpl.Common.BackendLLVM as CBE

translateToLLVM :: Syntax -> String
translateToLLVM syntax = syntax & number & extend translate & show

translate (a :< (Common common)) = CBE.translate a common
