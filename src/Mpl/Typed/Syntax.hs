module Mpl.Typed.Syntax where

import Data.Text             (Text)
import GHC.Generics          (Generic)
import Data.Functor.Foldable (Fix(..))

import qualified Mpl.Common.Syntax as CS

data SyntaxF r =
    Common (CS.SyntaxF r)
  | TypeAnnotation r Type
  deriving (Show, Generic, Eq)

data Type = TypeSymbol Text deriving (Show, Generic, Eq)

type Syntax = Fix SyntaxF

literal          = Fix . Common . CS.Literal
symbol           = Fix . Common . CS.Symbol
application a as = Fix . Common $ CS.Application a as

int                = literal . CS.IntegerLiteral
typeAnnotation a t = Fix (TypeAnnotation a t)
typeSymbol         = TypeSymbol
