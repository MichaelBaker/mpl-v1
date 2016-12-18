module Mpl.Utils
  ( Text
  , Generic
  , Annotated
  , Fix(..)
  , Cofree((:<))
  , textToString
  , stringToText
  , lazyTextToString
  , byteStringToString
  , stringToByteString
  , byteStringSlice
  , byteStringToText
  , jsIR
  ) where

import Data.Text                  (Text, pack, unpack)
import Control.Comonad.Cofree     (Cofree((:<)))
import Mpl.Annotation             (Annotated)
import Data.Functor.Foldable      (Fix(..))
import GHC.Generics               (Generic)
import Language.JavaScript.Parser (renderToText)

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text.Lazy as LT

textToString       = unpack
stringToText       = pack
byteStringToString = UTF8.toString
stringToByteString = UTF8.fromString
byteStringToText   = stringToText . byteStringToString

byteStringSlice startChar endChar byteString = UTF8.take spanSize $ UTF8.drop startChar byteString
  where spanSize = endChar - startChar

lazyTextToString = LT.unpack

jsIR = renderToText
