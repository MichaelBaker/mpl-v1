module Mpl.Typed.Core where

import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Typed.Syntax  as TS
import qualified Mpl.Common.Syntax as CS
import qualified Mpl.Common.Core   as CC
import qualified Prelude

data CoreF type_ binder recurse
  = Common (CC.CoreF binder recurse)
  | TypeAnnotation recurse type_
  deriving (Show, Generic, Functor, Eq)

data Binder type_ recurse
  = CommonBinder (CC.Binder recurse)
  | AnnotatedBinder recurse type_
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

data Type recurse
  = TypeSymbol Text
  | IntegerType
  | UTF8StringType
  | FunctionType recurse recurse
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable, Typeable, Data)

mapBinder :: (binder0 -> binder1)
          -> CoreF type_ binder0 recurse
          -> CoreF type_ binder1 recurse

mapBinder f (Common common) =
  Common (CC.mapBinder f common)

mapBinder _ (TypeAnnotation r t) = TypeAnnotation r t

mapType :: (type0 -> type1)
        -> CoreF type0 binder recurse
        -> CoreF type1 binder recurse

mapType _ (Common a) =
  Common a

mapType f (TypeAnnotation recurse type_) =
  TypeAnnotation recurse $ f type_

mapBinderType :: (type0 -> type1)
              -> Binder type0 recurse
              -> Binder type1 recurse

mapBinderType _ (CommonBinder a) =
  CommonBinder a

mapBinderType f (AnnotatedBinder recurse type_) =
  AnnotatedBinder recurse $ f type_
