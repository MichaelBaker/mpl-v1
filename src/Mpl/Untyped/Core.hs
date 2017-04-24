module Mpl.Untyped.Core where

import Mpl.Utils (Base, Generic, Cofree ((:<)), envcata)

import qualified Mpl.Untyped.Syntax as US
import qualified Mpl.Common.Core    as CC
import qualified Mpl.Common.Syntax  as CS

data CoreF binder recurse
  = Common (CC.CoreF binder recurse)
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

syntaxToCore span (US.Common common) =
  CC.syntaxToCore Common span common
