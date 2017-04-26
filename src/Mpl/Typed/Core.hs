module Mpl.Typed.Core where

import Data.Text       (Text)
import Mpl.Utils       (Fix(..), Cofree ((:<)), envcata)
import Mpl.ParserUtils (SourceAnnotated, SourceSpan)
import GHC.Generics    (Generic)

import qualified Mpl.Typed.Syntax  as TS
import qualified Mpl.Common.Syntax as CS
import qualified Mpl.Common.Core   as CC

data CoreF binder recurse
  = Common (CC.CoreF binder recurse)
  | TypeAnnotation recurse Type
  deriving (Show, Generic, Functor, Eq)

data Binder recurse
  = CommonBinder (CC.Binder recurse)
  | AnnotatedBinder recurse Type
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

data Type
  = TypeSymbol Text
  deriving (Show, Generic, Eq)

instance CC.ToCommonCore CoreF where
  toCommonCore = Common

instance CC.ToCoreBinder (SourceAnnotated TS.Binder) (SourceAnnotated Binder) where
  toCoreBinder = envcata convertBinder

syntaxToCore :: SourceSpan
             -> TS.SyntaxF
                  (SourceAnnotated TS.Binder)
                  (SourceAnnotated (CoreF (SourceAnnotated Binder)))
             -> (SourceAnnotated (CoreF (SourceAnnotated Binder)))
syntaxToCore span (TS.Common common) =
  CC.syntaxToCore span common

syntaxToCore span (TS.TypeAnnotation r t) =
  span :< TypeAnnotation r (convertType t)

convertBinder span (TS.CommonBinder (CS.Binder text)) =
  span :< CommonBinder (CC.Binder text)

convertBinder span (TS.AnnotatedBinder r t) =
  span :< AnnotatedBinder r (convertType t)

convertType (TS.TypeSymbol text) =
  TypeSymbol text
