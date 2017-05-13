module Mpl.Utils
  ( Annotated
  , Base
  , annotation
  , envcata
  , jsIR
  , mapAnnotated
  ) where

import Mpl.Annotation             (Annotated, Base, mapAnnotated, envcata, annotation)
import Language.JavaScript.Parser (renderToText)

jsIR = renderToText
