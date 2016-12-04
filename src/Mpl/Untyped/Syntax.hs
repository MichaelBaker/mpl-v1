module Mpl.Untyped.Syntax where

-- A dynamic lanugage with as few compile-time restrictions as possible.
-- For experienced programmers, this should provide a language in which to do fast scripting and experimentation.
-- For beginning programmers, it should provide a fertile learning environment where there are relatively few concepts to learn.

import Mpl.Utils (Generic, Fix(..))

import qualified Mpl.Common.Syntax as CS

data SyntaxF r = Common (CS.SyntaxF r) deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

type Syntax = Fix SyntaxF

literal =
  Fix . Common . CS.Literal
symbol =
  Fix . Common . CS.Symbol
function parameters body =
  Fix . Common $ CS.Function parameters body
application func arguments =
  Fix . Common $ CS.Application func arguments
leftAssociative =
  Fix . Common . CS.LeftAssociative
rightAssociative =
  Fix . Common . CS.RightAssociative

int = literal . CS.IntegerLiteral
