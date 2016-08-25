module Mpl.Dyn.Interpreter where

import Prelude hiding (concat, span)

import Mpl.Dyn.AST   (AST(..), span, emptySpan)
import Data.Text     (Text, pack, unpack, concat)
import Data.List     (foldl', intercalate)
import Control.Monad (foldM)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict                  as Map

data Value =
    VInt     Integer
  | VReal    Double
  | VUtf16   Text
  | VList    [Value]
  | VRec     (Map.Map FieldLabel Value)
  | VClosure AST Env
  deriving (Show)

data FieldLabel =
    FSym Text
  | FInt Integer
  deriving (Show, Eq, Ord)

type Env = Map.Map Text Value

data InterpreterState = InterpreterState
  { env :: Env
  }

interpret :: AST -> Text
interpret ast = pack $ showValue $ State.evalState (interp ast) startState
  where startState = InterpreterState { env = Map.empty }

interp :: AST -> State.State InterpreterState Value
interp (AInt a _) = return $ VInt a
interp (AReal a _) = return $ VReal a
interp (AUtf16 a _) = return $ VUtf16 a
interp (AList as lspan) = do
  vals <- mapM interp as
  return $ VList vals
interp (ARec fs rspan) = foldM (addRecField rspan) (VRec Map.empty) fs
interp a@(AField _ _ _) = return $ VUtf16 $ concat ["<Invalid expression: '", pack (show a), "'>"]
interp (ASym name symSpan) = do
  state <- State.get
  return $ case Map.lookup name (env state) of
    Nothing -> VUtf16 (concat ["<Undefined symbol '", name, "'>"])
    Just a  -> a
interp (ALet bindings body letSpan) = interpWithBindings letSpan body bindings
interp a@(ALam _ _ _) = do
  state <- State.get
  return $ VClosure a (env state)
interp app@(AApp fAst args _) = do
  f <- interp fAst
  case f of
    VClosure lam@(ALam params body _) localEnv -> do
      State.withState (\s -> s { env = localEnv }) $ addLambdaBindings app lam body params args
    a -> return $ VUtf16 (concat ["<Tried to call ", pack (showValue a), " as a function at ", pack (show $ span app)])
interp a = return undefined -- TODO: Make this total

addLambdaBindings app lam body [] [] = do
  interp body
addLambdaBindings app lam _ [] _ = do
  return $ VUtf16 (concat ["<Too many arguments provided to ", pack (showCode lam), " in ", pack (showCode app), ">"])
addLambdaBindings app lam body ps [] = do
  state <- State.get
  return $ VClosure (ALam ps body (span lam)) (env state)
addLambdaBindings app lam body (p:ps) (a:as) = do
  case p of
    ASym name _ -> do
      addBinding name a
      addLambdaBindings app lam body ps as
    a -> return $ VUtf16 (concat ["<Invalid parameter: ", pack (showCode p), ">"])

interpWithBindings span body ((ADef (ASym name _) valueAst _):rest) = do
  addBinding name valueAst
  interpWithBindings span body rest
interpWithBindings span body (def:_) = do
  return $ VUtf16 $ concat ["<Invalid definition: '", pack (show def), "' in ", pack (show span), ">"]
interpWithBindings span body [] = interp body

addRecField recSpan (VRec fields) (AField (ASym label _) valueAst _) = do
  value <- interp valueAst
  return $ VRec (Map.insert (FSym label) value fields)
addRecField recSpan (VRec fields) (AField (AInt label _) valueAst _) = do
  value <- interp valueAst
  return $ VRec (Map.insert (FInt label) value fields)
addRecField recSpan rec field = do
  let message = concat ["<Invalid record field '", pack (show field), "' at ", pack $ show recSpan, ">"]
  return $ VUtf16 message

addBinding name body = do
  value <- interp body
  State.modify' $ \state -> state { env = Map.insert name value (env state) }

showValue (VInt a)         = show a
showValue (VReal a)        = show a
showValue (VUtf16 a)       = show a
showValue (VList as)       = "[" ++ intercalate ", " (map showValue as) ++ "]"
showValue (VRec as)        = "{" ++ intercalate ", " (map showRecField $ Map.toList as) ++ "}"
showValue (VClosure lam _) = showCode lam

showCode (ALam ps body _) =
  let paramList = if null ps then "" else unwords (map showCode ps) ++ " = "
  in "(# " ++ paramList ++ showCode body ++ ")"
showCode (ASym a _) = unpack a
showCode (AInt a _) = show a
showCode a = show a

showRecField (FSym name, value) = unpack name ++ ": " ++ showValue value
showRecField (FInt name, value) = show name   ++ ": " ++ showValue value

