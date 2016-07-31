module Mpl.Dyn.AST where

import GHC.Generics     (Generic)
import Data.Text        (Text)
import Text.Show.Pretty (Value(String), PrettyVal, prettyVal, dumpStr)
import Data.Int         (Int64(..))

data AST =
    AProg  [AST]     Span
  | AInt   Integer   Span
  | AReal  Double    Span
  | ADef   AST AST   Span
  | ASym   Text      Span
  | ALam   [AST] AST Span
  deriving (Generic, Eq)

data Span = Span
  { filePath  :: String
  , startByte :: Int64
  , endByte   :: Int64
  } deriving (Show)

emptySpan = Span "" 0 0

instance Eq Span where
  a == b = True

instance PrettyVal AST

instance Show AST where
  show = dumpStr

instance PrettyVal Text where
  prettyVal = String . show

instance PrettyVal Span where
  prettyVal = String . show
