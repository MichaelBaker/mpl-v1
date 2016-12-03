module Mpl.Utils
  ( Text
  , Generic
  , Annotated
  , Fix(..)
  , Cofree((:<))
  , textToString
  , stringToText
  , lazyTextToString
  , llvmIR
  , jsIR
  ) where

import Data.Text                  (Text, pack, unpack)
import Control.Comonad.Cofree     (Cofree((:<)))
import Mpl.Annotation             (Annotated)
import Data.Functor.Foldable      (Fix(..))
import GHC.Generics               (Generic)
import Control.Monad.IO.Class     (liftIO)
import LLVM.General.Context       (withContext)
import LLVM.General.Module        (withModuleFromAST, moduleLLVMAssembly)
import Control.Monad.Except       (ExceptT, runExceptT)
import Control.Monad              ((>=>))
import Language.JavaScript.Parser (renderToText)

import qualified Data.Text.Lazy as LT

textToString = unpack
stringToText = pack

lazyTextToString = LT.unpack

llvmIR llvmModule = do
  withContext $ \context -> do
    liftError $ withModuleFromAST context llvmModule $ \llvmModule -> do
      liftIO $ moduleLLVMAssembly llvmModule

jsIR = renderToText

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return
