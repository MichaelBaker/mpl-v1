module Mpl.Interpreter where

import Mpl.Parser    (AST(..), parse)
import Data.Text     (Text, pack, unpack)
import Data.Typeable (Typeable)
import Text.Earley   (Report)
import Data.Dynamic  (Dynamic, toDyn, fromDynamic)
import Data.List     (foldl')
import Data.List     (intercalate)
import Data.Maybe    (fromJust)
import Numeric       (showFFloatAlt)
import qualified Data.Map.Strict as Map

interpret :: String -> String
interpret string = case run string of
  Left  s -> s
  Right s -> s

run :: String -> Either String String
run string = do
  parsedAST <- Right $ parse (pack string)
  ast       <- handleParseFail parsedAST
  result    <- Right $ exec emptyStack ast
  return $ show result

exec :: Stack -> AST () -> Object
exec stack (AIdent _ a) = case lookupIdent a stack of
  Nothing -> Object TError $ toDyn $ "Invalid identifier: " ++ (show a)
  Just o  -> o
exec stack (AFunc  _ as a) = Object TFunc   $ toDyn ((as, stack, a) :: (AST (), Stack, AST ()))
exec stack (AInt   _ a)    = Object TInt    $ toDyn (a :: Integer)
exec stack (AFloat _ a)    = Object TFloat  $ toDyn (a :: Double)
exec stack (AList  _ as)   = Object TList   $ toDyn $ map (exec stack) as
exec stack (AMap   _ as)   = Object TMap    $ toDyn $ Map.fromList $ map (\(a, b) -> (exec stack a, exec stack b)) as
exec stack (AText  _ a)    = Object TText   $ toDyn a
exec stack (AApp   _ a b)  = applyFunction stack a b

applyFunction stack f as = case exec stack f of
  Object TFunc val -> let (AList _ params, closureStack, body) = coerce val :: (AST (), Stack, AST ())
                          numParams = length params
                          numArgs   = length as
                          in if numParams /= numArgs
                            then Object TError $ toDyn ("Function " ++ show f ++ " takes " ++ show numParams ++ " parameters, but " ++ show numArgs ++ " arguments were provided" :: String)
                            else let newStack = Map.fromList $ zip (paramNames params) (execArgs stack as)
                              in exec (newStack:closureStack) body
  _ -> Object TError $ toDyn (show f ++ " is not a function" :: String)

paramNames = map (\(AIdent _ a) -> a)
execArgs stack = map (exec stack)

handleParseFail :: ([AST ()], Report Text Text) -> Either String (AST ())
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r

data Object = Object Type Dynamic
type Env    = Map.Map Text Object
type Stack  = [Env]

emptyEnv   = Map.empty :: Env
emptyStack = [] :: Stack

lookupIdent _ []     = Nothing
lookupIdent i (env:envs) = case Map.lookup i env of
  Nothing -> lookupIdent i envs
  Just a  -> Just a

data Type = TInt
          | TFloat
          | TMap
          | TList
          | TFun
          | TText
          | TError
          | TFunc

coerce :: (Typeable a) => Dynamic -> a
coerce = fromJust . fromDynamic

instance Show Object where
  show (Object TInt v)    = show (coerce v :: Integer)
  show (Object TFloat v)  = showFFloatAlt Nothing (coerce v :: Double) ""
  show (Object TMap v)    = "{" ++ (if null fields then fields else init (init fields)) ++ "}"
    where map    = coerce v :: Map.Map Object Object
          fields = Map.foldlWithKey' showField "" map
          showField acc k v = acc ++ show k ++ ": " ++ show v ++ ", "
  show (Object TList v)   = "[" ++ if null values then values else init (init values)  ++ "]"
    where list   = coerce v :: [Object]
          values = foldl' (\acc v -> acc ++ show v ++ ", ") "" list
  show (Object TFun v)    = show "<Function>"
  show (Object TText v)   = "\"" ++ (unpack $ coerce v) ++ "\""
  show (Object TError v)  = "Error: " ++ (coerce v)
  show (Object TFunc v)   = let (params, _, body) = coerce v  :: (AST (), Stack, AST ()) in "#(" ++ show params ++ " " ++ show body ++ ")"

instance Eq Object where
  (Object TInt v1)    == (Object TInt v2)    = (coerce v1 :: Integer)               == (coerce v2 :: Integer)
  (Object TFloat v1)  == (Object TFloat v2)  = (coerce v1 :: Double)                == (coerce v2 :: Double)
  (Object TMap v1)    == (Object TMap v2)    = (coerce v1 :: Map.Map Object Object) == (coerce v2 :: Map.Map Object Object)
  (Object TList v1)   == (Object TList v2)   = (coerce v1 :: [Object])              == (coerce v2 :: [Object])
  (Object TText v1)   == (Object TText v2)   = (coerce v1 :: Text)                  == (coerce v2 :: Text)
  _                   == _                   = False

instance Ord Object where
  (Object TInt v1)    `compare` (Object TInt v2)    = (coerce v1 :: Integer)               `compare` (coerce v2 :: Integer)
  (Object TFloat v1)  `compare` (Object TFloat v2)  = (coerce v1 :: Double)                `compare` (coerce v2 :: Double)
  (Object TMap v1)    `compare` (Object TMap v2)    = (coerce v1 :: Map.Map Object Object) `compare` (coerce v2 :: Map.Map Object Object)
  (Object TList v1)   `compare` (Object TList v2)   = (coerce v1 :: [Object])              `compare` (coerce v2 :: [Object])
  (Object TText v1)   `compare` (Object TText v2)   = (coerce v1 :: Text)                  `compare` (coerce v2 :: Text)
  _                   `compare` _                   = EQ
