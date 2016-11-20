module Mpl.Common.BackendLLVM where

import Mpl.Common.Syntax
import LLVM.General.AST
import LLVM.General.AST.Global
import Mpl.Utils                 (Cofree((:<)), Annotated, textToString)
import LLVM.General.AST.Constant (Constant(Int))
import LLVM.General.AST.Type     (Type(IntegerType))
import LLVM.General.AST.Operand  (Operand)

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

translate (Literal literal) = [translateLiteral literal]
translate (Symbol sym) = [LocalReference (IntegerType 32) $ Name (textToString sym)]
translate (Application f as) = concat (tag f : map tag as)

translateLiteral (IntegerLiteral i) = ConstantOperand (Int 32 i)

tag (i :< _, [LocalReference a (Name name)]) = [LocalReference a $ Name (name ++ "_" ++ show i)]
tag (_, a) = a
