module Mpl.Untyped.BackendLLVM where

import LLVM.General.AST

import Data.Function           ((&))
import LLVM.General.AST
import LLVM.General.AST.Global
import Mpl.Annotation          (Cofree((:<)), number, envcata)
import Mpl.ParserUtils         (Parsed)
import Mpl.Untyped.Syntax      (SyntaxF(..))

import qualified Mpl.Common.BackendLLVM as CBE

translateToLLVM :: Parsed SyntaxF -> Module
translateToLLVM syntax =
  let (instructions, operand) = syntax & number & envcata translate
      mainFunction =
        functionDefaults
          { returnType = IntegerType 32
          , name = Name "main"
          , basicBlocks =
              [BasicBlock
                (Name "_0")
                (reverse instructions)
                (Do $ Ret (Just operand) [])]
          }
  in
    Module
      { moduleName         = "Main"
      , moduleDataLayout   = Nothing
      , moduleTargetTriple = Nothing
      , moduleDefinitions  = [GlobalDefinition mainFunction]
      }


translate i (Common common) = CBE.translate i common
