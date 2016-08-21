module Mpl.Dyn.Interpreter where

import Prelude hiding (concat, span)

import Mpl.Dyn.AST (AST(..), span, emptySpan)
import Data.Text   (Text, pack, unpack, concat)
import Data.List   (foldl', intercalate)

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
interp a@(AUtf16 _ _) = return a
interp (AList as lspan) = do
  vals <- mapM interp as
  return $ AList vals lspan
interp (ARec fs rspan) = do
  case filter (not . isValidField) fs of
    (invalidField:_) ->
      let invalidSpan = span invalidField
          invalidCode = showResult invalidField
          message     = concat ["Invalid record field '", pack invalidCode, "' at ", pack $ show invalidSpan]
          in return $ AUtf16 message invalidSpan
    _ -> do
      newFields <- mapM interp fs
      return $ ARec newFields rspan
interp (AField key@(ASym _ _) val fspan) = do
  newVal <- interp val
  return $ AField key newVal fspan
interp (AField key val fspan) = do
  newKey <- interp key
  newVal <- interp val
  return $ AField newKey newVal fspan
interp a@(ASym name symSpan) = do
  state <- State.get
  return $ case Map.lookup name (env state) of
    Nothing -> AUtf16 (concat ["Undefined symbol '", name, "'"]) symSpan
    Just a  -> a
interp a@(ALet bindings body _) = do
  case filter (not . isValidDef) bindings of
    (invalidDef:_) ->
      let invalidSpan = span invalidDef
          invalidCode = showResult invalidDef
          message     = concat ["Invalid definition'", pack invalidCode, "' at ", pack $ show invalidSpan]
          in return $ AUtf16 message invalidSpan
    _ -> do
      mapM_ (addBinding . defToPair) bindings
      interp body
interp a@(ALam _ _ _) = return a
interp a = return undefined -- TODO: Make this total

defToPair (ADef sym@(ASym _ _) body _) = (sym, body)
defToPair a = error $ "Invalid definition: " ++ show a

addBinding (ASym name _, body) = do
  value <- interp body
  State.modify' $ \state -> state { env = Map.insert name value (env state) }
addBinding a = error $ "Invalid binding: " ++ show a

isValidField (AField (ASym _ _) _ _) = True
isValidField (AField (AInt _ _) _ _) = True
isValidField _                       = False

isValidDef (ADef (ASym _ _) _ _) = True
isValidDef _                     = False

isValidParam (ASym _ _) = True
isValidParam _          = False

showResult (AInt a _) = show a
showResult (AReal a _) = show a
showResult (AUtf16 a _) = show a
showResult (AList as _) = "[" ++ intercalate ", " (map showResult as) ++ "]"
showResult (ARec as _) = "{" ++ intercalate ", " (map showResult as) ++ "}"
showResult (AField key val _) = showResult key ++ ": " ++ showResult val
showResult (ASym a _) = unpack a
showResult (ALam ps body _) =
  let paramList = if null ps then "" else unwords (map showResult ps) ++ " = "
  in "(# " ++ paramList ++ showResult body ++ ")"
showResult a = error $ "Cannot showResult: '" ++ show a ++ "'"  -- TODO: Make this total
