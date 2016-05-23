module Mpl.Object where

import Data.Text       (Text, unpack)
import Data.Dynamic    (Dynamic, toDyn, fromDynamic)
import Data.Typeable   (Typeable)
import Data.Maybe      (fromJust)
import Data.List       (foldl')
import Numeric         (showFFloatAlt)
import qualified Data.Map.Strict as Map

data Object = Object Type Dynamic

data Type = TInt
          | TFloat
          | TMap
          | TList
          | TFun
          | TString
          | TError

coerce :: (Typeable a) => Dynamic -> a
coerce = fromJust . fromDynamic

instance Show Object where
  show (Object TInt v)    = show (coerce v :: Integer)
  show (Object TFloat v)  = showFFloatAlt Nothing (coerce v :: Float) ""
  show (Object TMap v)    = "{" ++ (if null fields then fields else init (init fields)) ++ "}"
    where map    = coerce v :: Map.Map Object Object
          fields = Map.foldlWithKey' showField "" map
          showField acc k v = acc ++ show k ++ ": " ++ show v ++ ", "
  show (Object TList v)   = "[" ++ if null values then values else init (init values)  ++ "]"
    where list   = coerce v :: [Object]
          values = foldl' (\acc v -> acc ++ show v ++ ", ") "" list
  show (Object TFun v)    = show "<Function>"
  show (Object TString v) = "\"" ++ (unpack $ coerce v) ++ "\""
  show (Object TError v)  = "Error: " ++ (coerce v)

instance Eq Object where
  (Object TInt v1)    == (Object TInt v2)    = (coerce v1 :: Integer)               == (coerce v2 :: Integer)
  (Object TFloat v1)  == (Object TFloat v2)  = (coerce v1 :: Float)                 == (coerce v2 :: Float)
  (Object TMap v1)    == (Object TMap v2)    = (coerce v1 :: Map.Map Object Object) == (coerce v2 :: Map.Map Object Object)
  (Object TList v1)   == (Object TList v2)   = (coerce v1 :: [Object])              == (coerce v2 :: [Object])
  (Object TString v1) == (Object TString v2) = (coerce v1 :: Text)                  == (coerce v2 :: Text)
  _                   == _                   = False

instance Ord Object where
  (Object TInt v1)    `compare` (Object TInt v2)    = (coerce v1 :: Integer)               `compare` (coerce v2 :: Integer)
  (Object TFloat v1)  `compare` (Object TFloat v2)  = (coerce v1 :: Float)                 `compare` (coerce v2 :: Float)
  (Object TMap v1)    `compare` (Object TMap v2)    = (coerce v1 :: Map.Map Object Object) `compare` (coerce v2 :: Map.Map Object Object)
  (Object TList v1)   `compare` (Object TList v2)   = (coerce v1 :: [Object])              `compare` (coerce v2 :: [Object])
  (Object TString v1) `compare` (Object TString v2) = (coerce v1 :: Text)                  `compare` (coerce v2 :: Text)
  _                   `compare` _                   = EQ
