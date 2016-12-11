module Mpl.Typed.Syntax where

import Data.Text    (Text)
import GHC.Generics (Generic)

import qualified Mpl.Common.Syntax as CS

data SyntaxF r =
    Common (CS.SyntaxF r)
  | TypeAnnotation r Type
  deriving (Show, Generic, Functor, Eq)

data Type = TypeSymbol Text deriving (Show, Generic, Eq)


literal                    = Common . CS.literal
symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
leftAssociative            = Common . CS.leftAssociative
rightAssociative           = Common . CS.rightAssociative
int                        = Common . CS.int
typeAnnotation             = TypeAnnotation
typeSymbol                 = TypeSymbol
