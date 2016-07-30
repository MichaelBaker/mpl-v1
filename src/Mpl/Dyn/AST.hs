module Mpl.Dyn.AST where

import GHC.Generics     (Generic)
import Text.Show.Pretty (PrettyVal, dumpStr)

data AST =
  AInt Integer
  deriving (Generic, Eq)

instance PrettyVal AST

instance Show AST where
  show = dumpStr
