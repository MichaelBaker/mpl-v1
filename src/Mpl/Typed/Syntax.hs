module Mpl.Typed.Syntax where

import           Mpl.Prelude
import qualified Mpl.Common.Syntax as CS
import qualified Prelude

data SyntaxF type_ binder recurse
  = Common (CS.SyntaxF binder recurse)
  | TypeAnnotation recurse type_
  deriving (Show, Generic, Functor, Eq)

data Binder type_ recurse
  = CommonBinder (CS.Binder recurse)
  | AnnotatedBinder recurse type_
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

data Type recurse
  = TypeSymbol Text
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

literal                    = Common . CS.literal
symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
int                        = Common . CS.int
utf8String                 = Common . CS.utf8String
typeAnnotation             = TypeAnnotation
typeSymbol                 = TypeSymbol

binder          = CommonBinder . CS.binder
annotatedBinder = AnnotatedBinder

mapCommon :: (CS.SyntaxF binder0 recurse -> CS.SyntaxF binder1 recurse)
          -> SyntaxF type_ binder0 recurse
          -> SyntaxF type_ binder1 recurse

mapCommon f (Common a) =
  Common (f a)

mapCommon _ (TypeAnnotation recurse type_) =
  TypeAnnotation recurse type_

mapType :: (type0 -> type1)
        -> SyntaxF type0 binder recurse
        -> SyntaxF type1 binder recurse

mapType _ (Common a) =
  Common a

mapType f (TypeAnnotation recurse type_) =
  TypeAnnotation recurse $ f type_

mapBinderType :: (type0 -> type1)
              -> Binder type0 recurse
              -> Binder type1 recurse

mapBinderType _ (CommonBinder a) =
  CommonBinder a

mapBinderType f (AnnotatedBinder recurse type_) =
  AnnotatedBinder recurse $ f type_
