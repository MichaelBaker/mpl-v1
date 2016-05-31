module Mpl.Interpreter where

import Mpl.AST (Core(..), CoreType, meta)

import Data.Text     (Text, unpack)
import Data.Typeable (Typeable)
import Data.Dynamic  (Dynamic, toDyn, fromDynamic)
import Data.List     (foldl', intercalate)
import Data.Maybe    (fromJust)
import Numeric       (showFFloatAlt)

import qualified Data.Map.Strict as Map

data Object = Object ObjectType Dynamic
type Env    = Map.Map Text Object

data ObjectType = OTUnit
                | OTInt
                | OTReal
                | OTText
                | OTMap
                | OTList
                | OTThunk
                | OTFunc
                | OTError

interpret :: Core CoreType -> Object
interpret ast = exec Map.empty ast

exec :: Env -> Core CoreType -> Object
exec env (CIdent _ a) = case Map.lookup a env of
  Nothing -> Object OTError $ toDyn $ "Invalid identifier: " ++ (show a)
  Just o  -> o
exec env (CUnit   _)    = Object OTUnit  $ toDyn ()
exec env (CInt   _ a)   = Object OTInt   $ toDyn (a :: Integer)
exec env (CReal   _ a)  = Object OTReal  $ toDyn (a :: Double)
exec env (CText   _ a)  = Object OTText  $ toDyn (a :: Text)
exec env (CList   _ as) = Object OTList  $ toDyn (map (exec env) as :: [Object])
exec env (CMap   _ as)  = Object OTMap   $ toDyn (Map.fromList $ map (\(k, v) -> (exec env k, exec env v)) as :: Map.Map Object Object)
exec env (CThunk _ a)   = Object OTThunk $ toDyn ((a, env) :: (Core CoreType, Env))
exec env (CFunc _ a b)  = Object OTFunc  $ toDyn ((a, b, env) :: ((Text, CoreType), Core CoreType, Env))
exec env (CForce _ a)   = forceThunk     $ exec env a
exec env (CApp _ f a)   = evalFunction (exec env f) (exec env a)

coerce :: (Typeable a) => Dynamic -> a
coerce = fromJust . fromDynamic

forceThunk (Object OTThunk v) = let (body, env) = coerce v :: (Core CoreType, Env) in exec env body
forceThunk o = Object OTError $ toDyn ("Tried to force something that isn't a thunk: " ++ show o :: String)

evalFunction (Object OTFunc v) arg =
  let ((param, _), body, env) = coerce v :: ((Text, CoreType), Core CoreType, Env)
    in exec (Map.insert param arg env) body
evalFunction o _ = Object OTError $ toDyn ("Tried to apply something that isn't a function: " ++ show o :: String)

instance Show Object where
  show (Object OTUnit _)   = "()"
  show (Object OTInt v)    = show (coerce v :: Integer)
  show (Object OTReal v)   = showFFloatAlt Nothing (coerce v :: Double) ""
  show (Object OTThunk v)  = "<Thunk>"
  show (Object OTFunc v)   = "<Function>"
  show (Object OTText v)   = "\"" ++ (unpack $ coerce v) ++ "\""
  show (Object OTError v)  = "Error: " ++ (coerce v)
  show (Object OTMap v)    = "{" ++ (if null fields then fields else init (init fields)) ++ "}"
    where map    = coerce v :: Map.Map Object Object
          fields = Map.foldlWithKey' showField "" map
          showField acc k v = acc ++ show k ++ ": " ++ show v ++ ", "
  show (Object OTList v)   = "[" ++ values  ++ "]"
    where list   = coerce v :: [Object]
          values = intercalate ", " $ map show list

showCore (CIdent _ a) = unpack a
showCore (CList _ as) = "[" ++ (intercalate ", " $ map showCore as) ++ "]"
showCore a            = show a

instance Eq Object where
  (Object OTUnit _)    == (Object OTUnit _)    = True
  (Object OTInt v1)    == (Object OTInt v2)    = (coerce v1 :: Integer)               == (coerce v2 :: Integer)
  (Object OTReal v1)   == (Object OTReal v2)   = (coerce v1 :: Double)                == (coerce v2 :: Double)
  (Object OTMap v1)    == (Object OTMap v2)    = (coerce v1 :: Map.Map Object Object) == (coerce v2 :: Map.Map Object Object)
  (Object OTList v1)   == (Object OTList v2)   = (coerce v1 :: [Object])              == (coerce v2 :: [Object])
  (Object OTText v1)   == (Object OTText v2)   = (coerce v1 :: Text)                  == (coerce v2 :: Text)
  _                    == _                    = False

instance Ord Object where
  (Object OTInt v1)    `compare` (Object OTInt v2)    = (coerce v1 :: Integer)               `compare` (coerce v2 :: Integer)
  (Object OTReal v1)   `compare` (Object OTReal v2)   = (coerce v1 :: Double)                `compare` (coerce v2 :: Double)
  (Object OTMap v1)    `compare` (Object OTMap v2)    = (coerce v1 :: Map.Map Object Object) `compare` (coerce v2 :: Map.Map Object Object)
  (Object OTList v1)   `compare` (Object OTList v2)   = (coerce v1 :: [Object])              `compare` (coerce v2 :: [Object])
  (Object OTText v1)   `compare` (Object OTText v2)   = (coerce v1 :: Text)                  `compare` (coerce v2 :: Text)
  _                    `compare` _                   = EQ
