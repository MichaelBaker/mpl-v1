module Mpl.Common.Syntax where

-- Language features that are common to all languages.
-- It provides a powerful, reusable starting point on which to add incremental features when making new languages.

import Mpl.Utils (Text, Generic, Fix(..))

data SyntaxF r =
    Literal     Literal
  | Symbol      Text
  | Application r [r]
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

data Literal = IntegerLiteral Integer deriving (Show, Generic, Eq)
