module Mpl.Dyn.Core where

import GHC.Generics     (Generic)
import Data.Text        (Text)
import Text.Show.Pretty (Value(String), PrettyVal, prettyVal, dumpStr)
import Mpl.Span         (Span)

data Core =
    CSym Text Span
  deriving (Generic, Eq)

instance PrettyVal Core

instance Show Core where
  show = dumpStr

instance PrettyVal Text where
  prettyVal = String . show
