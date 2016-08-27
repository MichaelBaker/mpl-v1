module Mpl.Dyn.Core where

import GHC.Generics     (Generic)
import Data.Text        (Text)
import Text.Show.Pretty (Value(String), PrettyVal, prettyVal, dumpStr)
import Mpl.Span         (Span)

data Core =
    CSym   Text                    Span
  | CInt   Integer                 Span
  | CReal  Double                  Span
  | CUtf16 Text                    Span
  | CList  [Core]                  Span
  | CRec   RecordImpl              Span
  | CLet   [(CoreBind, Core)] Core Span
  deriving (Generic, Eq)

type RecordImpl = [(CoreLabel, Core)]

data CoreBind = CoreBind Text
  deriving (Generic, Eq, Ord)

data CoreLabel =
    CLSym Text
  | CLInt Integer
  deriving (Generic, Eq, Ord)

instance PrettyVal Core
instance Show Core where
  show = dumpStr

instance PrettyVal CoreLabel
instance Show CoreLabel where
  show = dumpStr

instance PrettyVal CoreBind
instance Show CoreBind where
  show = dumpStr

instance PrettyVal Text where
  prettyVal = String . show
