module Mpl.Utils
  ( Text
  , Generic
  , Annotated
  , Fix(..)
  , Cofree((:<))
  , textToString
  , stringToText
  ) where

import Data.Text              (Text, pack, unpack)
import Control.Comonad.Cofree (Cofree((:<)))
import Mpl.Annotation         (Annotated)
import Data.Functor.Foldable  (Fix(..))
import GHC.Generics           (Generic)

textToString = unpack
stringToText = pack
