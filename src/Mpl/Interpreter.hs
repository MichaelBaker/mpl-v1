module Mpl.Interpreter where

import Mpl.AST    (AST(..), Type, meta)

import Data.Text     (Text, unpack)
import Data.Typeable (Typeable)
import Data.Dynamic  (Dynamic, toDyn, fromDynamic)
import Data.List     (foldl')
import Data.Maybe    (fromJust)
import Numeric       (showFFloatAlt)
import qualified Data.Map.Strict as Map

interpret :: AST Type -> Object
interpret ast = exec emptyStack ast

exec :: Stack -> AST Type -> Object
exec stack (AIdent _ a) = case lookupIdent a stack of
  Nothing -> Object OTError $ toDyn $ "Invalid identifier: " ++ (show a)
  Just o  -> o
exec stack (AFunc  _ as a) = Object OTFunc   $ toDyn ((as, stack, a) :: ([AST Type], Stack, AST Type))
exec stack (AInt   _ a)    = Object OTInt    $ toDyn (a :: Integer)
exec stack (AFloat _ a)    = Object OTFloat  $ toDyn (a :: Double)
exec stack (AList  _ as)   = Object OTList   $ toDyn $ map (exec stack) as
exec stack (AMap   _ as)   = Object OTMap    $ toDyn $ Map.fromList $ map (\(a, b) -> (exec stack a, exec stack b)) as
exec stack (AText  _ a)    = Object OTText   $ toDyn a
exec stack (AApp   _ a b)  = applyFunction stack a b

applyFunction stack f as = case exec stack f of
  Object OTFunc val ->
    let (params, closureStack, body) = coerce val :: ([AST Type], Stack, AST Type)
        numParams = length params
        numArgs   = length as
        in if numParams /= numArgs
          then Object OTError $ toDyn ("Function " ++ show f ++ " takes " ++ show numParams ++ " parameters, but " ++ show numArgs ++ " arguments were provided" :: String)
          else let newStack = Map.fromList $ zip (paramNames params) (execArgs stack as)
            in exec (newStack:closureStack) body
  _ -> Object OTError $ toDyn (show f ++ " is not a function" :: String)

paramNames = map (\(AIdent _ a) -> a)
execArgs stack = map (exec stack)

data Object = Object ObjectType Dynamic
type Env    = Map.Map Text Object
type Stack  = [Env]

emptyEnv   = Map.empty :: Env
emptyStack = [] :: Stack

lookupIdent _ []     = Nothing
lookupIdent i (env:envs) = case Map.lookup i env of
  Nothing -> lookupIdent i envs
  Just a  -> Just a

data ObjectType = OTInt
                | OTFloat
                | OTMap
                | OTList
                | OTFun
                | OTText
                | OTError
                | OTFunc

coerce :: (Typeable a) => Dynamic -> a
coerce = fromJust . fromDynamic

instance Show Object where
  show (Object OTInt v)    = show (coerce v :: Integer)
  show (Object OTFloat v)  = showFFloatAlt Nothing (coerce v :: Double) ""
  show (Object OTMap v)    = "{" ++ (if null fields then fields else init (init fields)) ++ "}"
    where map    = coerce v :: Map.Map Object Object
          fields = Map.foldlWithKey' showField "" map
          showField acc k v = acc ++ show k ++ ": " ++ show v ++ ", "
  show (Object OTList v)   = "[" ++ if null values then values else init (init values)  ++ "]"
    where list   = coerce v :: [Object]
          values = foldl' (\acc v -> acc ++ show v ++ ", ") "" list
  show (Object OTFun v)    = show "<Function>"
  show (Object OTText v)   = "\"" ++ (unpack $ coerce v) ++ "\""
  show (Object OTError v)  = "Error: " ++ (coerce v)
  show (Object OTFunc v)   = let (params, _, body) = coerce v  :: ([AST Type], Stack, AST Type) in "#(" ++ show params ++ " " ++ show body ++ ")"

instance Eq Object where
  (Object OTInt v1)    == (Object OTInt v2)    = (coerce v1 :: Integer)               == (coerce v2 :: Integer)
  (Object OTFloat v1)  == (Object OTFloat v2)  = (coerce v1 :: Double)                == (coerce v2 :: Double)
  (Object OTMap v1)    == (Object OTMap v2)    = (coerce v1 :: Map.Map Object Object) == (coerce v2 :: Map.Map Object Object)
  (Object OTList v1)   == (Object OTList v2)   = (coerce v1 :: [Object])              == (coerce v2 :: [Object])
  (Object OTText v1)   == (Object OTText v2)   = (coerce v1 :: Text)                  == (coerce v2 :: Text)
  _                   == _                   = False

instance Ord Object where
  (Object OTInt v1)    `compare` (Object OTInt v2)    = (coerce v1 :: Integer)               `compare` (coerce v2 :: Integer)
  (Object OTFloat v1)  `compare` (Object OTFloat v2)  = (coerce v1 :: Double)                `compare` (coerce v2 :: Double)
  (Object OTMap v1)    `compare` (Object OTMap v2)    = (coerce v1 :: Map.Map Object Object) `compare` (coerce v2 :: Map.Map Object Object)
  (Object OTList v1)   `compare` (Object OTList v2)   = (coerce v1 :: [Object])              `compare` (coerce v2 :: [Object])
  (Object OTText v1)   `compare` (Object OTText v2)   = (coerce v1 :: Text)                  `compare` (coerce v2 :: Text)
  _                   `compare` _                   = EQ
