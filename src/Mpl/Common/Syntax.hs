module Mpl.Common.Syntax where

import Data.Text    (Text)
import GHC.Generics (Generic)

data SyntaxF r =
    Literal Literal
  | Symbol  Text
  | Application r [r]
  deriving (Show, Generic, Functor, Eq)

data Literal = IntegerLiteral Integer deriving (Show, Generic, Eq)
