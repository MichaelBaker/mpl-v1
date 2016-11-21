module Mpl.Untyped.BackendLLVM where

import LLVM.General.AST

import Mpl.Untyped.Syntax    (Syntax, SyntaxF(..))
import Mpl.Annotation        (Cofree((:<)), number, envcata)
import Data.Function         ((&))
import LLVM.General.AST
import LLVM.General.AST.Global

import qualified Mpl.Common.BackendLLVM as CBE

translateToLLVM :: Syntax -> Module
translateToLLVM syntax =
  let (instructions, _) = syntax & number & envcata translate
      mainFunction =
        functionDefaults
          { returnType = VoidType
          , name = Name "main"
          , basicBlocks =
              [BasicBlock
                (Name "_0")
                (reverse instructions)
                (Do $ Ret Nothing [])]
          }
  in
    Module
      { moduleName         = "Main"
      , moduleDataLayout   = Nothing
      , moduleTargetTriple = Nothing
      , moduleDefinitions  = [GlobalDefinition mainFunction]
      }


translate i (Common common) = CBE.translate i common
