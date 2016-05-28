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

eval :: String -> String
eval string = case run string of
                Left  s -> s
                Right s -> s

run :: String -> Either String String
run string = do
  parsedAST <- Right $ parse (pack string)
  ast       <- handleParseFail parsedAST
  result    <- Right $ exec ast
  return $ show result

exec :: AST -> Object
exec (AApp    a b) = Object TError  $ toDyn ("Function application not implemented yet" :: String)
exec (AIdent  a)   = Object TError  $ toDyn ("Identifiers not implemented yet" :: String)
exec (AInt    a)   = Object TInt    $ toDyn (read $ unpack a :: Integer)
exec (AFloat  a)   = Object TFloat  $ toDyn (read $ unpack a :: Float)
exec (AList   as)  = Object TList   $ toDyn $ map exec as
exec (AMap    as)  = Object TMap    $ toDyn $ Map.fromList $ map (\(a, b) -> (exec a, exec b)) as
exec (AText   a)   = Object TText   $ toDyn a

handleParseFail :: ([AST], Report Text Text) -> Either String AST
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r

data Object = Object Type Dynamic

data Type = TInt
          | TFloat
          | TMap
          | TList
          | TFun
          | TText
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
  show (Object TText v)   = "\"" ++ (unpack $ coerce v) ++ "\""
  show (Object TError v)  = "Error: " ++ (coerce v)

instance Eq Object where
  (Object TInt v1)    == (Object TInt v2)    = (coerce v1 :: Integer)               == (coerce v2 :: Integer)
  (Object TFloat v1)  == (Object TFloat v2)  = (coerce v1 :: Float)                 == (coerce v2 :: Float)
  (Object TMap v1)    == (Object TMap v2)    = (coerce v1 :: Map.Map Object Object) == (coerce v2 :: Map.Map Object Object)
  (Object TList v1)   == (Object TList v2)   = (coerce v1 :: [Object])              == (coerce v2 :: [Object])
  (Object TText v1)   == (Object TText v2)   = (coerce v1 :: Text)                  == (coerce v2 :: Text)
  _                   == _                   = False

instance Ord Object where
  (Object TInt v1)    `compare` (Object TInt v2)    = (coerce v1 :: Integer)               `compare` (coerce v2 :: Integer)
  (Object TFloat v1)  `compare` (Object TFloat v2)  = (coerce v1 :: Float)                 `compare` (coerce v2 :: Float)
  (Object TMap v1)    `compare` (Object TMap v2)    = (coerce v1 :: Map.Map Object Object) `compare` (coerce v2 :: Map.Map Object Object)
  (Object TList v1)   `compare` (Object TList v2)   = (coerce v1 :: [Object])              `compare` (coerce v2 :: [Object])
  (Object TText v1)   `compare` (Object TText v2) = (coerce v1 :: Text)                  `compare` (coerce v2 :: Text)
  _                   `compare` _                   = EQ
