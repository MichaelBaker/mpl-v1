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
  , Data
  , Eff
  , Either(..)
  , Eq
  , Functor
  , Generic
  , IO
  , Int
  , Integer
  , Maybe(..)
  , Member
  , Monad
  , Monoid
  , Ord
  , Show
  , String
  , Traversable
  , Typeable
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
  , run
  , send
  , show
  , showChar
  , showParen
  , showString
  , showsPrec
  , snd
  , toConstr
  , unzip
  , Text
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
  , unzip
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

