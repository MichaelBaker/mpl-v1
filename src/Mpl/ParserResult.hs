{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- This is a customized version of the Trifecta Result, but I feel compelled
-- to retain the original Copyright statement as a recognition that it is
-- almost entirely copied from the original.
--
-- |
-- Copyright   :  (c) Edward Kmett 2011-2015
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Results and Parse Errors
-----------------------------------------------------------------------------

module Mpl.ParserResult
  ( Result(..)
  , AsResult(..)
  , SyntaxError(..)
  , SpecificError(..)
  , Errable(..)
  , failed
  ) where

import Control.Applicative as Alternative
import Control.Lens hiding (snoc, cons)
import Control.Monad (guard)
import Data.ByteString as Strict hiding (empty, snoc)
import Data.Foldable
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup
import Data.Set as Set hiding (empty, toList)
import Data.Text (Text)
import Mpl.ParserDescription
import Mpl.Rendering
import Mpl.Utils (textToString)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Text.Trifecta.Delta as Delta
import Text.Trifecta.Rendering hiding (render)
import qualified Data.List as List

data SyntaxError =
  SyntaxError
    { errDelta    :: Delta
    , errExpected :: Set ParserDescription
    , errSpecific :: SpecificError
    }
    deriving (Show)

data SpecificError =
    NoError
  | BasicError
      String -- Error Message
  | ParserDescriptionError
      Doc -- Original Code
      ParserDescription
  | SuggestionError
      Doc -- Item Name
      Doc -- Expectation
      Doc -- Example
      Doc -- Original Code
  deriving (Show)

failed :: String -> Delta -> SyntaxError
failed message delta =
  SyntaxError
    { errDelta    = delta
    , errExpected = mempty
    , errSpecific = BasicError message
    }
{-# INLINE failed #-}

instance Semigroup SyntaxError where
  (SyntaxError a1 b1 c1) <> (SyntaxError a2 b2 c2) =
    SyntaxError
      (a1 <> a2)
      (b1 <> b2)
      (c1 <> c2)
  {-# INLINE (<>) #-}

instance Monoid SyntaxError where
  mempty = SyntaxError mempty mempty mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance Semigroup SpecificError where
  NoError <> error = error
  error <> NoError = error
  a@(ParserDescriptionError {}) <> _ = a
  a@(SuggestionError        {}) <> _ = a
  _ <> b = b
  {-# INLINE (<>) #-}

instance Monoid SpecificError where
  mempty = NoError
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

class Errable m where
  raiseErr :: SyntaxError -> m a

-- | The result of parsing. Either we succeeded or something went wrong.
data Result a
  = Success a
  | Failure SyntaxError
  deriving (Show,Functor,Foldable,Traversable)

-- | A 'Prism' that lets you embed or retrieve a 'Result' in a potentially larger type.
class AsResult s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Result :: Prism s t (Result a) (Result b)

instance AsResult (Result a) (Result b) a b where
  _Result = id
  {-# INLINE _Result #-}

instance Applicative Result where
  pure = Success
  {-# INLINE pure #-}
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure y = Failure y
  Failure x <*> Success _ = Failure x
  Failure x <*> Failure y = Failure (x <> y)
  {-# INLINE (<*>) #-}

instance Alternative Result where
  Failure x <|> Failure y = Failure (x <>y)
  Success a <|> Success _ = Success a
  Success a <|> Failure _ = Success a
  Failure _ <|> Success a = Success a
  {-# INLINE (<|>) #-}
  empty = Failure mempty
  {-# INLINE empty #-}
