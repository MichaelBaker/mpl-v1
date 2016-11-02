module Mpl.Common.BackendLLVM where

import Mpl.Common.Syntax
import LLVM.General.AST
import LLVM.General.AST.Global

mainFunction = functionDefaults
  { returnType = VoidType
  , name = Name "myFunction"
  }

testModule = Module
  { moduleName         = "MyModule"
  , moduleDataLayout   = Nothing
  , moduleTargetTriple = Nothing
  , moduleDefinitions  = [GlobalDefinition mainFunction]
  }

translate (Literal _) = testModule
translate (Symbol _) = testModule
translate (Application _ _) = testModule
