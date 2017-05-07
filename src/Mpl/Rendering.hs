module Mpl.Rendering
  ( module Mpl.Rendering
  , hardline
  )
  where

import           Mpl.Prelude
import           Mpl.Utils
import           Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Trifecta.Delta          as T
import qualified Data.ByteString.UTF8         as UTF8

(<~>) = (<>)

stack = vsep

indent = P.indent 2

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
callout_ = underline

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

dropFromEnd n byteString =
  UTF8.take (UTF8.length byteString - n) byteString

columnOf delta = fromIntegral (T.column delta)

columnBefore delta = max 0 $ fromIntegral (T.column delta) - 1

instance Pretty Text where
  pretty = pretty . textToString

instance Pretty ByteString where
  pretty = pretty . byteStringToString

