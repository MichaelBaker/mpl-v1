module Mpl.Dyn.AST where

import GHC.Generics     (Generic)
import Data.Text        (Text)
import Text.Show.Pretty (Value(String), PrettyVal, prettyVal, dumpStr)

data AST =
    AProg  [AST]
  | AInt   Integer
  | AReal  Double
  | ADef   AST AST
  | ASym   Text
  | ALam   [AST] AST
  deriving (Generic, Eq)

instance PrettyVal AST

instance Show AST where
  show = dumpStr

instance PrettyVal Text where
  prettyVal = String . show
