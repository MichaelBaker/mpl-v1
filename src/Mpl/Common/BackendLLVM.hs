module Mpl.Common.BackendLLVM where

import Mpl.Common.Syntax
import LLVM.General.AST
import LLVM.General.AST.Global

data LLVMState = LLVMState Integer

emptyLLVMState = LLVMState 0

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

