module Mpl.Span where

import Text.Show.Pretty (Value(String), PrettyVal, prettyVal)
import Data.Int         (Int64(..))

data Span = Span
  { filePath  :: String
  , startByte :: Int64
  , endByte   :: Int64
  } deriving (Show)

emptySpan = Span "" 0 0

instance Eq Span where
  a == b = True

instance PrettyVal Span where
  prettyVal = String . show
