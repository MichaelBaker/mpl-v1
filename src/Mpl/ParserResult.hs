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
  , _Success
  , Err(..), HasErr(..), Errable(..)
  , failed
  , errorMessage
  ) where

import Control.Applicative as Alternative
import Control.Lens hiding (snoc, cons)
import Control.Monad (guard)
import Data.ByteString as Strict hiding (empty, snoc)
import Data.Foldable
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup
import Data.Set as Set hiding (empty, toList)
import Mpl.ParserDescription
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)
import Text.Trifecta.Delta as Delta
import Text.Trifecta.Rendering
import Mpl.Utils (byteStringToString)
import qualified Data.List as List

-- | This is used to report an error. What went wrong, some supplemental docs and a set of things expected
-- at the current location. This does not, however, include the actual location.
data Err = Err
  { _reason        :: Maybe Doc
  , _footnotes     :: [Doc]
  , _expected      :: Set ParserDescription
  , _finalDeltas   :: [Delta]
  , _errSuggestion :: Maybe ParserSuggestion
  , _errByteString :: Maybe ByteString
  , _errDelta      :: Maybe Delta
  }
  deriving (Show)

makeClassy ''Err

errorMessage error =
  case _errSuggestion error of
    Just a  -> errorSuggestion a (_errByteString error)
    Nothing -> ""

errorSuggestion suggestion maybeOriginal = unlines (header ++ examples)
  where item = itemName suggestion
        header =
          [ "Found a syntatically incorrect " ++ item ++ "."
          , ""
          , expectation suggestion
          ]
        examples =
          case maybeOriginal of
            Nothing ->
              [ "Here is an example of a syntactically correct " ++ item
              , ""
              , example suggestion
              ]
            Just original ->
              [ "Here is the " ++ item ++ " as you wrote it and an example that is syntactically correct:"
              , ""
              , "Original:"
              , "  " ++ byteStringToString original
              , ""
              , "Example:"
              , "  " ++ example suggestion
              ]

failed :: String -> Delta -> ByteString -> Err
failed m delta byteString= Err (Just (pretty m)) [] mempty mempty Nothing (Just byteString) (Just delta)
{-# INLINE failed #-}

instance Semigroup Err where
  Err md mds mes delta1 s1 ebs1 edelta1 <> Err nd nds nes delta2 s2 ebs2 edelta2 =
    Err
      (nd <|> md)
      (if isJust nd then nds else if isJust md then mds else nds ++ mds)
      (mes <> nes)
      (delta1 <> delta2)
      (if isJust s1 then s1 else s2)
      (ebs1 <> ebs2)
      (edelta1 <> edelta2)
  {-# INLINE (<>) #-}

instance Monoid Err where
  mempty = Err Nothing [] mempty mempty Nothing mempty mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

class Errable m where
  raiseErr :: Err -> m a

-- | The result of parsing. Either we succeeded or something went wrong.
data Result a
  = Success a
  | Failure Err
  deriving (Show,Functor,Foldable,Traversable)

-- | A 'Prism' that lets you embed or retrieve a 'Result' in a potentially larger type.
class AsResult s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Result :: Prism s t (Result a) (Result b)

instance AsResult (Result a) (Result b) a b where
  _Result = id
  {-# INLINE _Result #-}

-- | The 'Prism' for the 'Success' constructor of 'Result'
_Success :: AsResult s t a b => Prism s t a b
_Success = _Result . dimap seta (either id id) . right' . rmap (fmap Success) where
  seta (Success a) = Right a
  seta (Failure e) = Left (pure (Failure e))
{-# INLINE _Success #-}

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
