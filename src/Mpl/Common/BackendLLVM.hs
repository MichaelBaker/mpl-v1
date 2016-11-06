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

translate :: Integer -> SyntaxF a -> ([()], Operand)
translate name (Literal literal) = translateLiteral literal
translate name (Symbol sym) = ([], LocalReference (IntegerType 32) $ Name (show name ++ textToString sym))
translate _ _ = ([], ConstantOperand (Int 32 123))

translateLiteral (IntegerLiteral i) = ([], ConstantOperand (Int 32 i))
