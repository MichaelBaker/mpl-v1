module Mpl.Dyn.Interpreter where

import Prelude hiding (concat, span)

import Mpl.Span      (emptySpan)
import Mpl.Dyn.Core  (Core(..), CoreLabel(..), CoreBind(..))
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
  | VRec     (Map.Map ValueLabel Value)
  | VClosure Core Env
  deriving (Show)

data ValueLabel =
    VLSym Text
  | VLInt Integer
  deriving (Show, Eq, Ord)

type Env = Map.Map Text Value

data InterpreterState = InterpreterState
  { env :: Env
  }

interpret :: Core -> Text
interpret ast = pack $ showValue $ State.evalState (interp ast) startState
  where startState = InterpreterState { env = Map.empty }

interp :: Core -> State.State InterpreterState Value
interp (CInt a _) = return $ VInt a
interp (CReal a _) = return $ VReal a
interp (CUtf16 a _) = return $ VUtf16 a
interp (CList as lspan) = do
  vals <- mapM interp as
  return $ VList vals
interp (CRec fs rspan) = do
  State.mapState
    (\(recordImpl, state) -> (VRec recordImpl, state))
    $ foldM (addRecField rspan) Map.empty fs
interp (CSym name symSpan) = do
  state <- State.get
  return $ case Map.lookup name (env state) of
    Nothing -> VUtf16 (concat ["<Undefined symbol '", name, "'>"])
    Just a  -> a
interp (CLet bindings body letSpan) = interpWithBindings letSpan body bindings
-- interp a@(ALam _ _ _) = do
--   state <- State.get
--   return $ VClosure a (env state)
-- interp app@(AApp fAst args _) = do
--   f <- interp fAst
--   case f of
--     VClosure lam@(ALam params body _) localEnv -> do
--       State.withState (\s -> s { env = localEnv }) $ addLambdaBindings app lam body params args
--     a -> return $ VUtf16 (concat ["<Tried to call ", pack (showValue a), " as a function at ", pack (show $ span app)])

-- addLambdaBindings app lam body [] [] = do
--   interp body
-- addLambdaBindings app lam _ [] _ = do
--   return $ VUtf16 (concat ["<Too many arguments provided to ", pack (showCode lam), " in ", pack (showCode app), ">"])
-- addLambdaBindings app lam body ps [] = do
--   state <- State.get
--   return $ VClosure (ALam ps body (span lam)) (env state)
-- addLambdaBindings app lam body (p:ps) (a:as) = do
--   case p of
--     ASym name _ -> do
--       addBinding name a
--       addLambdaBindings app lam body ps as
--     a -> return $ VUtf16 (concat ["<Invalid parameter: ", pack (showCode p), ">"])

addRecField recSpan fields (label, core) = do
  let valueLabel = case label of
                     CLSym labelName -> VLSym labelName
                     CLInt labelName -> VLInt labelName
  State.mapState (transform valueLabel) (interp core)
  where transform valueLabel (value, state) = (Map.insert valueLabel value fields, state)

interpWithBindings span body ((CoreBind name, core):rest) = do
  addBinding name core
  interpWithBindings span body rest
interpWithBindings span body [] = interp body

addBinding name core = do
  value <- interp core
  State.modify' $ \state -> state { env = Map.insert name value (env state) }

showValue (VInt a)         = show a
showValue (VReal a)        = show a
showValue (VUtf16 a)       = show a
showValue (VList as)       = "[" ++ intercalate ", " (map showValue as) ++ "]"
showValue (VRec as)        = "{" ++ intercalate ", " (map showRecField $ Map.toList as) ++ "}"
-- showValue (VClosure lam _) = showCode lam
showValue a = error $ "Cannow show value: " ++ show a -- TODO: Make this total

-- showCode (ALam ps body _) =
--   let paramList = if null ps then "" else unwords (map showCode ps) ++ " = "
--   in "(# " ++ paramList ++ showCode body ++ ")"
-- showCode (ASym a _) = unpack a
-- showCode (AInt a _) = show a
-- showCode a = show a

showRecField (VLSym name, value) = unpack name ++ ": " ++ showValue value
showRecField (VLInt name, value) = show name   ++ ": " ++ showValue value
