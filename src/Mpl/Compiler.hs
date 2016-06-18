module Mpl.Compiler where

import Mpl.AST         (AST)
import Mpl.Core        (Core(..))
import Mpl.ASTToCore   (astToCore)
import Mpl.Parser      (parse)
import Mpl.Interpreter (Val(..), RuntimeError(..), interpret)
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
  core      <- astToCore ast

  case interpret core of
    Left  e -> Left  $ printError e
    Right v -> Right $ printVal v

handleParseFail :: ([AST], Report Text Text) -> Either String (AST)
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r

printVal (Core c)      = printCore c
printVal (Closure _ c) = printCore c

printError (AppliedNonThunk core)    = "Tried to force something that isn't a thunk: " ++ printCore core
printError (AppliedNonFunction core) = "Tried to apply something that isn't a function: " ++ printCore core
printError (UnboundIdentifier text)  = "Unbound identifier: " ++ unpack text

printCore (CInt   _ i)   = show i
printCore (CIdent _ t)   = unpack t
printCore (CThunk _ b)   = "(# []" ++ printCore b ++ ")"
printCore (CForce _ t)   = "(" ++ printCore t ++ ")"
printCore (CFunc  _ p b) = "(# [" ++ unpack p ++ "] " ++ printCore b ++ ")"
printCore (CApp   _ f a) = "(" ++ printCore f ++ " " ++ printCore a ++ ")"
