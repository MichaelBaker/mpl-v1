module Mpl.Span where

import Text.Show.Pretty          (Value(String), PrettyVal, prettyVal)
import Data.Int                  (Int64(..))
import Text.Trifecta.Combinators (DeltaParsing(), position)
import Data.ByteString.Char8     (unpack)
import Text.Trifecta.Delta       (Delta(Directed, Columns, Tab, Lines))
import Text.Trifecta.Combinators (DeltaParsing())

data Span = Span
  { filePath  :: String
  , startByte :: Int64
  , endByte   :: Int64
  } deriving (Show)

instance Eq Span where
  a == b = True

instance PrettyVal Span where
  prettyVal = String . show

emptySpan = Span "" 0 0
zeroDelta = Columns 0 0

getPosition :: (DeltaParsing m) => m Delta
getPosition = position

withSpan :: (DeltaParsing m) => m (Span -> a) -> m a
withSpan parser = do
  startSpan <- position
  item      <- parser
  endSpan   <- position
  return $ item (makeSpan startSpan endSpan)

makeSpan startSpan endSpan = Span (unpack filePath) startByte endByte
  where (filePath, startByte) = case startSpan of
                                  Columns _ bytes           -> ("<no file>", bytes)
                                  Tab _ _ bytes             -> ("<no file>", bytes)
                                  Lines _ _ bytes _         -> ("<no file>", bytes)
                                  Directed file _ _ bytes _ -> (file, bytes)
        endByte = case endSpan of
                    Columns _ bytes        -> bytes
                    Tab _ _ bytes          -> bytes
                    Lines _ _ bytes _      -> bytes
                    Directed _ _ _ bytes _ -> bytes

