module Mpl.Dyn.Interpreter where

import Prelude hiding (concat)

import Mpl.Dyn.AST (AST(..), emptySpan)
import Data.Text   (Text, pack, unpack, concat)
import Data.List   (intercalate)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict                  as Map

data InterpreterState = InterpreterState
  { env :: Map.Map Text AST
  }

interpret :: AST -> Text
interpret ast = pack $ showResult $ State.evalState (interp ast) startState
  where startState = InterpreterState { env = Map.empty }

interp :: AST -> State.State InterpreterState AST
interp a@(AInt _ _) = return a
interp a@(AReal _ _) = return a
interp a@(AList as lspan) = do
  vals <- mapM interp as
  return $ AList vals lspan
interp a@(ASym name symSpan) = do
  state <- State.get
  return $ case Map.lookup name (env state) of
    Nothing -> AUtf16 (concat ["Undefined symbol '", name, "'"]) symSpan
    Just a  -> a
interp a = return undefined -- TODO: Make this total

showResult (AInt a _) = show a
showResult (AReal a _) = show a
showResult (AUtf16 a _) = unpack a
showResult (AList as _) = "[" ++ intercalate ", " (map showResult as) ++ "]"
showResult a = error $ "Cannot showResult: '" ++ show a ++ "'"  -- TODO: Make this total
