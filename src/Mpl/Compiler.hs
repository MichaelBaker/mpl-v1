module Mpl.Compiler where

import Mpl.AST         (AST)
import Mpl.Core        (Core(..))
import Mpl.ASTToCore   (astToCore)
import Mpl.Parser      (parse)
import Mpl.Interpreter (RuntimeError(..), interpret)
import Text.Earley     (Report)
import Data.Text       (Text, unpack, pack)
import Data.List       (intercalate)

import Prelude hiding (curry)

data Error
  = PE String
  | AC String
  | RE RuntimeError
  deriving (Show)

compile :: String -> Either Error String
compile string = do
  ast  <- handleParseFail     $ parse (pack string)
  core <- handleASTToCoreFail $ astToCore ast
  handleInterpretFail $ interpret core

handleParseFail :: ([AST], Report Text Text) -> Either Error AST
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ PE $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ PE $ show r

handleASTToCoreFail (Right core) = Right core
handleASTToCoreFail (Left  e)    = Left $ AC e

handleInterpretFail (Right c) = Right $ printCore c
handleInterpretFail (Left e)  = Left  $ RE e

printCore (CInt   _ i)   = show i
printCore (CIdent _ t)   = unpack t
printCore (CThunk _ b)   = "(# []" ++ printCore b ++ ")"
printCore (CForce _ t)   = "(" ++ printCore t ++ ")"
printCore (CFunc  _ p b) = "(# [" ++ unpack p ++ "] " ++ printCore b ++ ")"
printCore (CApp   _ f a) = "(" ++ printCore f ++ " " ++ printCore a ++ ")"
