module Mpl.Dyn.AST where

import GHC.Generics     (Generic)
import Data.Text        (Text)
import Text.Show.Pretty (Value(String), PrettyVal, prettyVal, dumpStr)
import Data.Int         (Int64(..))

data AST =
    AProg    AST          Span
  | ARecDefs [AST]        Span
  | ALet     [AST] AST    Span
  | ARec     [AST]        Span
  | AField   AST AST      Span
  | AInt     Integer      Span
  | AReal    Double       Span
  | AUtf16   Text         Span
  | ADef     AST AST      Span
  | ASym     Text         Span
  | ALam     [AST] AST    Span
  | AList    [AST]        Span
  | ALens    [AST]        Span
  | AApp     AST [AST]    Span
  deriving (Generic, Eq)

data Span = Span
  { filePath  :: String
  , startByte :: Int64
  , endByte   :: Int64
  } deriving (Show)

emptySpan = Span "" 0 0

span (AProg    _   s) = s
span (ARecDefs _   s) = s
span (ALet     _ _ s) = s
span (ARec     _   s) = s
span (AField   _ _ s) = s
span (AInt     _   s) = s
span (AReal    _   s) = s
span (AUtf16   _   s) = s
span (ADef     _ _ s) = s
span (ASym     _   s) = s
span (ALam     _ _ s) = s
span (AList    _   s) = s
span (ALens    _   s) = s
span (AApp     _ _ s) = s

instance Eq Span where
  a == b = True

instance PrettyVal AST

instance Show AST where
  show = dumpStr

instance PrettyVal Text where
  prettyVal = String . show

instance PrettyVal Span where
  prettyVal = String . show
