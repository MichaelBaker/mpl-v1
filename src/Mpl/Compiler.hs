module Mpl.Compiler where

import Mpl.AST         (AST)
import Mpl.Core        (Core(..))
import Mpl.ASTToCore   (astToCore)
import Mpl.Parser      (parse)
import Mpl.Interpreter (Val(..), interpret)
import Text.Earley     (Report)
import Data.Text       (Text, unpack, pack)
import Data.List       (intercalate)

import Prelude hiding (curry)

compile :: String -> String
compile string = case run string of
  Left  s -> s
  Right s -> s

run :: String -> Either String String
run string = do
  parsedAST <- Right $ parse (pack string)
  ast       <- handleParseFail parsedAST
  result    <- Right $ printVal $ interpret $ astToCore ast
  return result

handleParseFail :: ([AST ()], Report Text Text) -> Either String (AST ())
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r

printVal (Core c)      = prettyPrint c
printVal (Closure _ c) = prettyPrint c

prettyPrint (CInt   _ i)   = show i
prettyPrint (CIdent _ t)   = unpack t
prettyPrint (CThunk _ b)   = "(# []" ++ prettyPrint b ++ ")"
prettyPrint (CForce _ t)   = "(" ++ prettyPrint t ++ ")"
prettyPrint (CFunc  _ p b) = "(# [" ++ unpack p ++ "] " ++ prettyPrint b ++ ")"
prettyPrint (CApp   _ f a) = "(" ++ prettyPrint f ++ " " ++ prettyPrint a ++ ")"
