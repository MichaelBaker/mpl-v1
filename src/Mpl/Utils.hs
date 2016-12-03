module Mpl.Utils
  ( Text
  , Generic
  , Annotated
  , Fix(..)
  , Cofree((:<))
  , textToString
  , stringToText
  , lazyTextToString
  , jsIR
  ) where

import Data.Text                  (Text, pack, unpack)
import Control.Comonad.Cofree     (Cofree((:<)))
import Mpl.Annotation             (Annotated)
import Data.Functor.Foldable      (Fix(..))
import GHC.Generics               (Generic)
import Language.JavaScript.Parser (renderToText)

import qualified Data.Text.Lazy as LT

textToString = unpack
stringToText = pack

lazyTextToString = LT.unpack

jsIR = renderToText
