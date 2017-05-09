module Mpl.Utils
  ( Annotated
  , Base
  , Cofree((:<))
  , Fix(..)
  , Foldable
  , cata
  , jsIR
  , project
  , refix
  , mapAnnotated
  , envcata
  , annotation
  ) where

import Control.Comonad.Cofree     (Cofree((:<)))
import Mpl.Annotation             (Annotated, Base, mapAnnotated, envcata, annotation)
import Data.Functor.Foldable      (Fix(..), Foldable, cata, project, refix)
import Language.JavaScript.Parser (renderToText)
import Prelude hiding (Foldable)

jsIR = renderToText
