module Mpl.Compiler where

import Mpl.AST         (AST)
import Mpl.Core        (Core(..))
import Mpl.Parser      (toAST)
import Mpl.ASTToCore   (toCore)
import Mpl.Typing      (CaseFile, toTypedCore)
import Mpl.Interpreter (RuntimeError(..), toValue)
import Text.Earley     (Report)
import Data.Text       (Text, unpack, pack)
import Data.List       (intercalate)

data Error m
  = PE String
  | AC String
  | TE String
  | RE (RuntimeError m)
  deriving (Show)

data Options = Options
  { haltOnTypeErrors :: Bool }

compile :: String -> Options -> Either (Error CaseFile) String
compile string options = do
  ast       <- handleParseFail     $ toAST (pack string)
  core      <- handleASTToCoreFail $ toCore ast
  typedCore <- handleTypingFail (haltOnTypeErrors options) $ toTypedCore core
  handleInterpretFail $ toValue typedCore

handleParseFail :: ([AST], Report Text Text) -> Either (Error m) AST
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ PE $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ PE $ show r

handleASTToCoreFail (Right core) = Right core
handleASTToCoreFail (Left  e)    = Left $ AC e

handleInterpretFail (Right c) = Right $ printCore c
handleInterpretFail (Left e)  = Left  $ RE e

handleTypingFail False (_, typedCore)       = Right typedCore
handleTypingFail True (Nothing, typedCore)  = Right typedCore
handleTypingFail True (Just err, typedCore) = Left $ TE $ show err

printCore (CInt   _ _ i)   = show i
printCore (CIdent _ _ t)   = unpack t
printCore (CThunk _ _ b)   = "(# []" ++ printCore b ++ ")"
printCore (CForce _ _ t)   = "(" ++ printCore t ++ ")"
printCore (CFunc  _ _ p b) = "(# [" ++ unpack p ++ "] " ++ printCore b ++ ")"
printCore (CApp   _ _ f a) = "(" ++ printCore f ++ " " ++ printCore a ++ ")"

