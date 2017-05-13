module Mpl.Parser.SourceSpan where

import Mpl.Prelude
import Text.Trifecta.Delta

data SourceSpan =
  SourceSpan
    { startDelta :: Delta
    , startLine  :: ByteString
    , endDelta   :: Delta
    }
  deriving (Show, Eq, Typeable, Data)

emptySpan =
  SourceSpan
    { startDelta = mempty
    , startLine  = mempty
    , endDelta   = mempty
    }

spanUnion s0 s1 =
  SourceSpan
    { startDelta = startDelta s0
    , startLine  = startLine s0
    , endDelta   = endDelta s1
    }

