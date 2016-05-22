module Mpl.Object where

import Data.Dynamic    (Dynamic, toDyn, fromDynamic)
import Data.Maybe      (fromJust)
import Data.List       (foldl')
import qualified Data.Map.Strict as Map

data Object = Object Type Dynamic

data Type = TInt
          | TFloat
          | TMap
          | TList
          | TFun
          | TString
          | TError

instance Show Object where
  show (Object TInt v)    = show (fromJust $ fromDynamic v :: Integer)
  show (Object TFloat v)  = show (fromJust $ fromDynamic v :: Float)
  show (Object TMap v)    = "{" ++ (if null fields then fields else init fields) ++ "}"
    where map    = fromJust $ fromDynamic v :: Map.Map Object Object
          fields = Map.foldlWithKey' showField "" map
          showField acc k v = acc ++ show k ++ ": " ++ show v ++ ","
  show (Object TList v)   = "[" ++ if null values then values else init values  ++ "]"
    where list   = fromJust $ fromDynamic v :: [Object]
          values = foldl' (\acc v -> acc ++ show v ++ ",") "" list
  show (Object TFun v)    = show "<Function>"
  show (Object TString v) = fromJust $ fromDynamic v
  show (Object TError v)  = "Error: " ++ (fromJust $ fromDynamic v)

