module Mpl.Utils
  ( Annotated
  , Base
  , Cofree((:<))
  , Fix(..)
  , Foldable
  , Generic
  , Text
  , UTF8.ByteString
  , byteStringSlice
  , byteStringToString
  , byteStringToText
  , cata
  , jsIR
  , lazyTextToString
  , stringToByteString
  , stringToText
  , textToString
  , project
  , refix
  , mapAnnotated
  , showText
  , (|>)
  ) where

import Data.Text                  (Text, pack, unpack)
import Control.Comonad.Cofree     (Cofree((:<)))
import Mpl.Annotation             (Annotated, Base, mapAnnotated)
import Data.Functor.Foldable      (Fix(..), Foldable, cata, project, refix)
import GHC.Generics               (Generic)
import Language.JavaScript.Parser (renderToText)
import Prelude hiding (Foldable)

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text.Lazy       as LT

textToString       = unpack
stringToText       = pack
byteStringToString = UTF8.toString
stringToByteString = UTF8.fromString
byteStringToText   = stringToText . byteStringToString

showText :: (Show a) => a -> Text
showText = stringToText . show

byteStringSlice startChar endChar byteString = UTF8.take spanSize $ UTF8.drop startChar byteString
  where spanSize = endChar - startChar

lazyTextToString = LT.unpack

jsIR = renderToText

a |> f = f a
