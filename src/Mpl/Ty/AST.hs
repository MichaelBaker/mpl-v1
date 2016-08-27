module Mpl.Ty.AST where

import GHC.Generics     (Generic)
import Data.Text        (Text)
import Text.Show.Pretty (Value(String), PrettyVal, prettyVal, dumpStr)
import Mpl.Span         (Span)

import qualified Mpl.Dyn.AST as Dyn

data AST =
    AAnnExp AST TyAST Span
  | ADyn    Dyn.AST
  deriving (Generic, Eq)

data TyAST =
  ATySym Text Span
  deriving (Generic, Eq)

instance PrettyVal AST

instance Show AST where
  show = dumpStr

instance PrettyVal TyAST

instance Show TyAST where
  show = dumpStr
