module Mpl.ParserError where

import Data.Set              (Set)
import Data.Semigroup        (Semigroup, (<>))
import Mpl.ParserDescription (ParserDescription)
import Text.Trifecta.Delta   (Delta)

data Error state =
  Error
    { parserState   :: state
    , errorDelta    :: Delta
    , errorReason   :: Reason
    , errorExpected :: Set ParserDescription
    , errorType     :: ErrorType
    , errorEOF      :: Bool
    }
    deriving Show

data Reason
  = Message String
  | NoReason
  deriving (Show)

data ErrorType
  = EpsilonError
  | CommittedError
  deriving (Show)

instance Semigroup (Error state) where
  (Error a1 b1 c1 d1 e1 f1) <> (Error a2 b2 c2 d2 e2 f2) =
    Error a2 b2 c2 (d1 <> d2) (e1 <> e2) (f1 || f2)

instance (Monoid state) => Monoid (Error state) where
  mempty  = Error mempty mempty NoReason mempty mempty False
  mappend = (<>)

instance Semigroup ErrorType where
  EpsilonError <> a = a
  a <> EpsilonError = a
  a <> _            = a

instance Monoid ErrorType where
  mempty  = EpsilonError
  mappend = (<>)
