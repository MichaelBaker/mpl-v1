module Mpl.Parse where

import Text.Show.Pretty          (Value(String), PrettyVal, prettyVal)
import Data.Int                  (Int64(..))
import Text.Trifecta.Combinators (DeltaParsing(), position)
import Data.ByteString.Char8     (unpack)
import Text.Trifecta.Delta       (Delta(Directed, Columns, Tab, Lines))
import Text.Trifecta.Combinators (DeltaParsing())
import Text.Trifecta.Parser      (Parser(), parseFromFileEx, parseString)
import Text.Trifecta.Result      (Result())
import Control.Monad.IO.Class    (MonadIO())

data Span = Span
  { filePath  :: String
  , startByte :: Int64
  , endByte   :: Int64
  } deriving (Show)

instance Eq Span where
  a == b = True

instance PrettyVal Span where
  prettyVal = String . show

emptySpan       = Span "" 0 0
zeroDelta       = Columns 0 0

type FileParser   a b = a -> String -> IO (Result b)
type StringParser a b = a -> String -> Result b

parseFromFile :: (MonadIO m) => Parser a -> String -> m (Result a)
parseFromFile = parseFromFileEx

parseFromString = parseString

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

