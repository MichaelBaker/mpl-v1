module Mpl.Interpreter where

import Mpl.AST    (AST(..), Type, meta)

import Data.Text     (Text, unpack)
import Data.Typeable (Typeable)
import Data.Dynamic  (Dynamic, toDyn, fromDynamic)
import Data.List     (foldl', intercalate)
import Data.Maybe    (fromJust)
import Numeric       (showFFloatAlt)
import qualified Data.Map.Strict as Map

interpret :: AST Type -> Object
interpret ast = exec emptyEnv ast

exec :: Env -> AST Type -> Object
exec env (AIdent _ a) = case Map.lookup a env of
  Nothing -> Object OTError $ toDyn $ "Invalid identifier: " ++ (show a)
  Just o  -> o
exec env (AFunc  _ as a)  = error "Tried to interpret an uncurried function"
exec env (ACFunc _ p b)   = Object OTFunc   $ toDyn ((p, env, b) :: (Maybe Text, Env, AST Type))
exec env (AInt   _ a)     = Object OTInt    $ toDyn (a :: Integer)
exec env (AFloat _ a)     = Object OTFloat  $ toDyn (a :: Double)
exec env (AList  _ as)    = Object OTList   $ toDyn $ map (exec env) as
exec env (AMap   _ as)    = Object OTMap    $ toDyn $ Map.fromList $ map (\(a, b) -> (exec env a, exec env b)) as
exec env (AText  _ a)     = Object OTText   $ toDyn a
exec env (AApp   _ a b)   = error "Tried to intrepret an uncurried application"
exec env (ACApp  _ f a)   = applyFunction env f a


applyFunction env f arg = case exec env f of
  Object OTFunc val -> let (param, closureEnv, body) = coerce val :: (Maybe Text, Env, AST Type)
    in case (param, arg) of
      (Nothing, Nothing) -> exec closureEnv body
      (Just p, Just a)   -> exec (Map.insert p (exec env a) closureEnv) body
      (Nothing, Just a)  -> Object OTError $ toDyn ("Too many arguments provided to: " ++ show f)
      (Just _, Nothing)  -> Object OTFunc val
  a -> Object OTError $ toDyn (show a ++ " is not a function" :: String)

data Object = Object ObjectType Dynamic
type Env    = Map.Map Text Object
emptyEnv    = Map.empty :: Env

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
  show (Object OTList v)   = "[" ++ values  ++ "]"
    where list   = coerce v :: [Object]
          values = intercalate ", " $ map show list
  show (Object OTFun v)    = show "<Function>"
  show (Object OTText v)   = "\"" ++ (unpack $ coerce v) ++ "\""
  show (Object OTError v)  = "Error: " ++ (coerce v)
  show (Object OTFunc v)   = let (param, _, body) = coerce v  :: (Maybe Text, Env, AST Type)
                                 in case param of
                                      Nothing -> "#([]" ++ " " ++ showAST body ++ ")"
                                      Just p  -> "#([" ++ unpack p ++ "] " ++ showAST body ++ ")"

showAST (AIdent _ a) = unpack a
showAST (AList _ as) = "[" ++ (intercalate ", " $ map showAST as) ++ "]"
showAST a            = show a

instance Eq Object where
  (Object OTInt v1)    == (Object OTInt v2)    = (coerce v1 :: Integer)               == (coerce v2 :: Integer)
  (Object OTFloat v1)  == (Object OTFloat v2)  = (coerce v1 :: Double)                == (coerce v2 :: Double)
  (Object OTMap v1)    == (Object OTMap v2)    = (coerce v1 :: Map.Map Object Object) == (coerce v2 :: Map.Map Object Object)
  (Object OTList v1)   == (Object OTList v2)   = (coerce v1 :: [Object])              == (coerce v2 :: [Object])
  (Object OTText v1)   == (Object OTText v2)   = (coerce v1 :: Text)                  == (coerce v2 :: Text)
  _                    == _                   = False

instance Ord Object where
  (Object OTInt v1)    `compare` (Object OTInt v2)    = (coerce v1 :: Integer)               `compare` (coerce v2 :: Integer)
  (Object OTFloat v1)  `compare` (Object OTFloat v2)  = (coerce v1 :: Double)                `compare` (coerce v2 :: Double)
  (Object OTMap v1)    `compare` (Object OTMap v2)    = (coerce v1 :: Map.Map Object Object) `compare` (coerce v2 :: Map.Map Object Object)
  (Object OTList v1)   `compare` (Object OTList v2)   = (coerce v1 :: [Object])              `compare` (coerce v2 :: [Object])
  (Object OTText v1)   `compare` (Object OTText v2)   = (coerce v1 :: Text)                  `compare` (coerce v2 :: Text)
  _                    `compare` _                   = EQ
