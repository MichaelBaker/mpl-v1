module Mpl.Typed.Syntax where

import Data.Text    (Text)
import Mpl.Utils    (Fix(..), project)
import GHC.Generics (Generic)

import qualified Mpl.Common.Syntax as CS

data SyntaxF binder recurse
  = Common (CS.SyntaxF binder recurse)
  | TypeAnnotation recurse Type
  deriving (Show, Generic, Functor, Eq)

data Type
  = TypeSymbol Text
  deriving (Show, Generic, Eq)


literal                    = Common . CS.literal
symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
int                        = Common . CS.int
typeAnnotation             = TypeAnnotation
typeSymbol                 = TypeSymbol

binder                     = CS.binder

mapCommon f (Common a) = Common (f a)
mapCommon _ (TypeAnnotation recurse ty) = TypeAnnotation recurse ty
