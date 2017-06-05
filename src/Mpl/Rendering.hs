module Mpl.Rendering
  ( module Mpl.Rendering
  , Doc
  , Pretty
  , hardline
  , pretty
  )
  where

import           Mpl.Prelude
import           Mpl.Utils
import           Mpl.Parser.SourceSpan
import           Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Trifecta.Delta          as T
import qualified Data.ByteString.UTF8         as UTF8

(<~>) = (<>)

stack = vsep

indent = P.indent 2

text :: Text -> Doc
text = pretty

header :: (Pretty a) => a -> Doc
header = header_ . pretty
header_ = dullblue . underline

suggestedAddition :: (Pretty a) => a -> Doc
suggestedAddition = suggestedAddition_ . pretty
suggestedAddition_ = dullgreen

problem :: (Pretty a) => a -> Doc
problem  = problem_ . pretty
problem_ = dullred

callout :: (Pretty a) => a -> Doc
callout = callout_ . pretty
callout_ = dullblue

highlight byteString span highlighter =
      toDoc (upTo start byteString)
  <~> highlighter (between start end byteString)
  <~> toDoc (after end byteString)
  where start = startDelta span
        end   = endDelta span

blankLine = hardline <~> hardline

toDoc :: (Pretty a) => a -> Doc
toDoc = pretty

render :: (Show a) => a -> String
render = show

upTo delta byteString =
  UTF8.take (columnBefore delta) byteString

after delta byteString =
  UTF8.drop (columnOf delta) byteString

at delta byteString =
  UTF8.take 1 $ UTF8.drop (columnBefore delta) byteString

between startDelta endDelta byteString =
  let chars = columnOf endDelta - columnBefore startDelta
  in UTF8.take chars $ UTF8.drop (columnBefore startDelta) byteString

betweenExclusive startDelta endDelta byteString =
  let takeChars = columnBefore endDelta - columnOf startDelta
      dropChars = columnOf startDelta
  in UTF8.take takeChars $ UTF8.drop dropChars byteString

extractSpan span byteString =
  between (startDelta span) (endDelta span) byteString

dropFromEnd n byteString =
  UTF8.take (UTF8.length byteString - n) byteString

columnOf delta = fromIntegral (T.column delta)

columnBefore delta = max 0 $ fromIntegral (T.column delta) - 1

instance Pretty Text where
  pretty = pretty . textToString

instance Pretty ByteString where
  pretty = pretty . byteStringToString

instance (Functor f, Pretty (f Doc)) => Pretty (Fix f) where
  pretty = cata pretty

instance (Functor f, Pretty (f Doc)) => Pretty (Cofree f a) where
  pretty = cata pretty
