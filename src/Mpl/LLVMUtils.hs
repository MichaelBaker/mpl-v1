module Mpl.LLVMUtils where

import LLVM.General.Context       (withContext)
import LLVM.General.Module        (withModuleFromAST, moduleLLVMAssembly)
import Control.Monad.Except       (ExceptT, runExceptT)
import Control.Monad              ((>=>))
import Control.Monad.IO.Class     (liftIO)
import Foreign.Ptr                (FunPtr, castFunPtr)

import qualified LLVM.General.AST             as AST
import qualified LLVM.General.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> IO Int

run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

llvmIR llvmModule = do
  withContext $ \context -> do
    liftError $ withModuleFromAST context llvmModule $ \llvmModule -> do
      liftIO $ moduleLLVMAssembly llvmModule

llvmJIT llvmModule = do
  withContext $ \context -> do
    EE.withMCJIT context optLevel model ptrelim fastins $ \engine -> do
      liftError $ withModuleFromAST context llvmModule $ \llvmModule -> do
        EE.withModuleInEngine engine llvmModule $ \execModule -> do
          mainFn <- EE.getFunction execModule (AST.Name "main")
          case mainFn of
            Nothing -> return Nothing
            Just fn -> do
              result <- run fn
              return (Just $ show result)
  where
    optLevel = Just 2
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return
