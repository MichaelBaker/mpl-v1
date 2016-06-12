module Mpl.Core where

import Data.Text (Text)
import qualified Data.Map.Strict as Map

data Term
data Type
data TyOp
data Poly a
data Dep  a

data Core a where
  CInt      :: Integer -> Core Term
  CReal     :: Double  -> Core Term
  CText     :: Text    -> Core Term
  CSym      :: Text    -> Core Term
  CRecord   :: Map.Map Text (Core Term) -> Core Term

  CIntTy    :: Core Type
  CTyParam  :: Text -> Core Type
  CLamTy    :: Core Type -> Core Type -> Core Type
  CRecordTy :: Map.Map Text (Core Type) -> Core Type

  CLam      :: Text -> Core Type -> Core Term -> Core Term
  CTyOp     :: Text -> Core Type -> Core TyOp
  CPolyFunc :: (Show a, Eq a) => Text -> Core a -> Core (Poly a)
  CDepFunc  :: (Show a, Eq a) => Text -> Core a -> Core (Dep a)
  CTyAnn    :: Core Type -> Core Term -> Core Term

  CTermApp  :: Core Term -> Core Term -> Core Term
  CTyOpApp  :: Core TyOp -> Core Type -> Core Type
  CPolyApp  :: (Show a, Eq a) => Core (Poly a) -> Core Type -> Core a
  CDepApp   :: (Show a, Eq a) => Core (Dep a)  -> Core Term -> Core a

deriving instance Show Term
deriving instance Show Type
deriving instance Show TyOp
deriving instance (Show a) => Show (Core a)
deriving instance (Show a) => Show (Poly a)
deriving instance (Show a) => Show (Dep  a)

deriving instance Eq Term
deriving instance Eq Type
deriving instance Eq TyOp
deriving instance (Eq a) => Eq (Core a)
deriving instance (Eq a) => Eq (Poly a)
deriving instance (Eq a) => Eq (Dep  a)
