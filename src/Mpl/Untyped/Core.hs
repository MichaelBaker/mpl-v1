module Mpl.Untyped.Core where

import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Core    as CC
import qualified Mpl.Common.Syntax  as CS
import qualified Mpl.Untyped.Syntax as US
import qualified Prelude

data CoreF binder recurse
  = Common (CC.CoreF binder recurse)
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

mapBinder :: (a -> binder) -> CoreF a recurse -> CoreF binder recurse
mapBinder f (Common common) =
  Common (CC.mapBinder f common)
