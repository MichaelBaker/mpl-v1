module Mpl.Annotation
  ( module Mpl.Annotation
  , Cofree((:<))
  , Base
  ) where

-- Utilities for annotating ASTs

import Prelude hiding (Foldable)

import GHC.Generics             (Generic)
import Data.Functor.Foldable    (Base, Fix(..), Foldable(project), cata, refix)
import Control.Monad.State.Lazy (State, get, modify, evalState)
import Control.Comonad          (Comonad, extract)
import Control.Comonad.Cofree   (Cofree((:<)))

type Annotated a b = Cofree a b

type instance Base (Annotated f b) = f

instance (Functor f) => Foldable (Annotated f a) where
  project (_ :< f) = f

annotation (a :< _) = a

-- Makes both the environment and node of a Foldable Comonad available during a catamorphism
envcata :: (Functor s, Foldable (x s a), Comonad (x s)) => (a -> (Base (x s a)) c -> c) -> x s a -> c
envcata f x = f (extract x) $ fmap (envcata f) (project x)

type Fixed a = Fix (Base a)

discardAnnotation :: (Foldable a) => a -> Fix (Base a)
discardAnnotation = refix

annotateWithState :: (Traversable (Base f), Foldable f) => a -> (a -> a) -> f -> Annotated (Base f) a
annotateWithState initialState modifyState ast =
  let astAnnotatedWithState = cata ((annotateState modifyState) :<) ast
      stateProducingAst     = sequence astAnnotatedWithState
  in evalState stateProducingAst initialState

annotateState :: (a -> a) -> State a a
annotateState modifyState =
  do
    annotation <- get
    modify modifyState
    return annotation

number :: (Traversable (Base f), Foldable f) => f -> Annotated (Base f) Integer
number = annotateWithState 0 (+ 1)
