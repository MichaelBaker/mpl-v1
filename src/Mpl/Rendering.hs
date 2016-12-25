module Mpl.Rendering
  ( module Mpl.Rendering
  , hardline
  , toDoc
  )
  where

import Mpl.Utils
import Text.PrettyPrint.ANSI.Leijen as P

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

instance Pretty Text where
  pretty = pretty . textToString

instance Pretty ByteString where
  pretty = pretty . byteStringToString