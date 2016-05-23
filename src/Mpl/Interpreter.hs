module Mpl.Interpreter where

import Mpl.Parser   (AST(..), parse)
import Mpl.Object   (Object(..), Type(..))
import Data.Text    (Text, pack, unpack)
import Text.Earley  (Report)
import Data.Dynamic (toDyn)
import Data.List    (intercalate)
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
exec (AString a)   = Object TString $ toDyn a

handleParseFail :: ([AST], Report Text Text) -> Either String AST
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r
