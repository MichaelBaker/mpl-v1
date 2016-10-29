module Mpl.Common.Syntax where

import Data.Text             (Text())
import GHC.Generics          (Generic)
import Data.Functor.Foldable (Fix(..))

data SyntaxF r =
    Literal Literal
  | Symbol  Text
  | Application r [r]
  deriving (Show, Generic, Functor, Eq)

data Literal = IntegerLiteral Integer deriving (Show, Generic, Eq)

type Syntax = Fix SyntaxF

literal          = Fix . Literal
symbol           = Fix . Symbol
application a as = Fix (Application a as)

int = literal . IntegerLiteral
