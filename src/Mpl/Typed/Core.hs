module Mpl.Typed.Core where

import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Typed.Syntax  as TS
import qualified Mpl.Common.Syntax as CS
import qualified Mpl.Common.Core   as CC
import qualified Prelude

data CoreF binder recurse
  = Common (CC.CoreF binder recurse)
  | TypeAnnotation recurse Type
  deriving (Show, Generic, Functor, Eq)

data Binder recurse
  = CommonBinder (CC.Binder recurse)
  | AnnotatedBinder recurse Type
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

data Type
  = TypeSymbol Text
  deriving (Show, Generic, Eq)

mapBinder :: (a -> binder) -> CoreF a recurse -> CoreF binder recurse
mapBinder f (Common common) =
  Common (CC.mapBinder f common)

mapBinder _ (TypeAnnotation r t) = TypeAnnotation r t
