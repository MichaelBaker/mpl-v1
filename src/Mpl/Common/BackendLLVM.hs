module Mpl.Common.BackendLLVM where

import Mpl.Common.Syntax as CS
import LLVM.General.AST
import LLVM.General.AST.Global
import Mpl.Utils                 (Cofree((:<)), Annotated, textToString)
import LLVM.General.AST.Constant (Constant(Int))
import LLVM.General.AST.Type     (Type(IntegerType))
import LLVM.General.AST.Operand  (Operand)

import qualified LLVM.General.AST.CallingConvention as CC

translate i (Literal literal) = ([], translateLiteral literal)
translate i (Symbol sym) = ([], LocalReference (IntegerType 32) $ Name (textToString sym))
translate i (Application f as) =
  let (fInstructions, fOperand) = f
      (aInstructions, aOps) = unzip as
      previousInstructions = concat aInstructions ++ fInstructions
      returnName = intName i
      callOperand = LocalReference (IntegerType 32) returnName
      callInstruction = returnName := Call Nothing CC.C [] (Right fOperand) (map toArg aOps) [] []
      instructions = callInstruction : previousInstructions
  in (instructions, callOperand)
translate _ (CS.Function _ _) = error $ "TODO: translate common function into LLVM"

toArg op = (op, [])

translateLiteral (IntegerLiteral i) = ConstantOperand (Int 32 i)

intName i = Name ("_" ++ show i)
