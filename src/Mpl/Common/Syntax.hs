module Mpl.Common.Syntax where

-- Language features that are common to all languages.
-- It provides a powerful, reusable starting point on which to add incremental features when making new languages.

import Mpl.Utils (Text, Generic, Fix(..))

data SyntaxF r =
    Literal          Literal
  | Symbol           Text
  | Function         [r] r
  | Application      r [r]
  | LeftAssociative  r
  | RightAssociative r
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

data Literal = IntegerLiteral Integer deriving (Show, Generic, Eq)

literal                    = Literal
symbol                     = Symbol
function parameters body   = Function parameters body
application func arguments = Application func arguments
leftAssociative            = LeftAssociative
rightAssociative           = RightAssociative
int                        = literal . IntegerLiteral
