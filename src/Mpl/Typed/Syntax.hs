module Mpl.Typed.Syntax where

import           Data.Text         (Text)
import           Mpl.Prelude
import           Mpl.Utils         (Fix(..), project)
import qualified Mpl.Common.Syntax as CS
import qualified Prelude

data SyntaxF binder recurse
  = Common (CS.SyntaxF binder recurse)
  | TypeAnnotation recurse Type
  deriving (Show, Generic, Functor, Eq)

data Binder recurse
  = CommonBinder (CS.Binder recurse)
  | AnnotatedBinder recurse Type
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

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

binder          = CommonBinder . CS.binder
annotatedBinder = AnnotatedBinder

mapCommon :: (CS.SyntaxF t recurse -> CS.SyntaxF binder recurse) -> SyntaxF t recurse -> SyntaxF binder recurse
mapCommon f (Common a) = Common (f a)
mapCommon _ (TypeAnnotation recurse ty) = TypeAnnotation recurse ty
