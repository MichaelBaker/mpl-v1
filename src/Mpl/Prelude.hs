module Mpl.Prelude
  ( module Mpl.Prelude
  , ($)
  , (+)
  , (++)
  , (-)
  , (.)
  , (<$>)
  , (<)
  , (<*)
  , (==)
  , (>)
  , (>>)
  , (>>=)
  , (||)
  , Arr
  , Bool(..)
  , ByteString
  , Cofree((:<))
  , Data
  , Eff
  , Either(..)
  , Eq
  , Fix(..)
  , Foldable
  , Functor
  , Generic
  , IO
  , Int
  , Integer
  , Maybe(..)
  , Member
  , Monad
  , MonadIO
  , Monoid
  , Ord
  , Show
  , String
  , Text
  , Traversable
  , Typeable
  , cata
  , compare
  , concat
  , either
  , error
  , fail
  , fmap
  , fromIntegral
  , fst
  , handleRelay
  , id
  , liftIO
  , mappend
  , max
  , maybe
  , mempty
  , negate
  , not
  , otherwise
  , project
  , pure
  , read
  , refix
  , return
  , run
  , send
  , show
  , showChar
  , showParen
  , showString
  , showsPrec
  , snd
  , toConstr
  , undefined
  , unzip
  ) where

import Prelude
  ( ($)
  , (+)
  , (++)
  , (-)
  , (.)
  , (<$>)
  , (<)
  , (<*)
  , (==)
  , (>)
  , (>>)
  , (>>=)
  , (||)
  , Bool(..)
  , Either(..)
  , Eq
  , Functor
  , IO
  , Int
  , Integer
  , Maybe(..)
  , Monad
  , Monoid
  , Ord
  , Show
  , String
  , Traversable
  , compare
  , concat
  , either
  , error
  , fail
  , fmap
  , fromIntegral
  , fst
  , id
  , mappend
  , max
  , maybe
  , mempty
  , negate
  , not
  , otherwise
  , pure
  , read
  , return
  , show
  , showChar
  , showParen
  , showString
  , showsPrec
  , snd
  , undefined
  , unzip
  , lookup
  )

import Data.ByteString.UTF8
  (ByteString)

import Data.Text
  ( Text
  , pack
  , unpack
  )

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text.Lazy       as LT

import GHC.Generics
  (Generic)

import Data.Data
  ( Data
  , Typeable
  , toConstr
  )

import Control.Monad.Freer
  ( Arr
  , Eff
  , Member
  , handleRelay
  , run
  , send
  )

import Data.Function
  ((&))

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Control.Comonad.Cofree
  (Cofree((:<)))

import Data.Functor.Foldable
  ( Fix(..)
  , Foldable
  , cata
  , project
  , refix
  )

(|>) = (&)

textToString =
  unpack

stringToText =
  pack

byteStringToString =
  UTF8.toString

stringToByteString =
  UTF8.fromString

byteStringToText =
  stringToText . byteStringToString

showText :: (Show a) => a -> Text
showText = stringToText . show

byteStringSlice startChar endChar byteString =
  UTF8.take spanSize $ UTF8.drop startChar byteString
    where spanSize = endChar - startChar

lazyTextToString =
  LT.unpack

associativeLookup :: (Eq a) => a -> [(a, b)] -> Maybe b
associativeLookup = lookup

zipExtra :: [a] -> [b] -> ([(a, b)], Maybe (Either [a] [b]))
zipExtra = zipWithExtra (,)

zipWithExtra :: (a -> b -> c) -> [a] -> [b] -> ([c], Maybe (Either [a] [b]))
zipWithExtra _ [] [] =
  ([], Nothing)

zipWithExtra _ as [] =
  ([], Just (Left as))

zipWithExtra _ [] bs =
  ([], Just (Right bs))

zipWithExtra f (a:as) (b:bs) =
  let (newPairs, leftover) = zipWithExtra f as bs
  in  ((f a b) : newPairs, leftover)
