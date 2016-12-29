module Mpl.Typed.Syntax where

import Data.Text    (Text)
import GHC.Generics (Generic)

import qualified Mpl.Common.Syntax as CS

data SyntaxF recurse
  = Common (CS.SyntaxF recurse)
  | TypeAnnotation recurse Type
  deriving (Show, Generic, Functor, Eq)

data Type
  = TypeSymbol Text
  deriving (Show, Generic, Eq)


literal                    = Common . CS.literal
binder                     = Common . CS.binder
symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
int                        = Common . CS.int
typeAnnotation             = TypeAnnotation
typeSymbol                 = TypeSymbol
