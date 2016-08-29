module Mpl.Dyn.Interpreter where

import Prelude hiding (concat, span)

import Mpl.Parse     (emptySpan)
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
  | VLens    [ValueLabel]
  | VThunk   Core Env
  | VClosure CoreBind Core Env
  deriving (Show, Eq)

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
interp (CLens as _) = convertValsToLabels [] as
interp (CSym name symSpan) = do
  state <- State.get
  return $ case Map.lookup name (env state) of
    Nothing -> VUtf16 (concat ["<Undefined symbol '", name, "'>"])
    Just a  -> a
interp (CLet bindings body letSpan) = interpWithBindings letSpan body bindings
interp a@(CLam param body _) = do
  state <- State.get
  return $ VClosure param body (env state)
interp a@(CThunk body _) = do
  state <- State.get
  return $ VThunk body (env state)
interp app@(CApp fCore argCore _) = do
  f <- interp fCore
  case f of
    VLens as -> do
      arg <- interp argCore
      interpGetLens arg arg [] as
    VClosure (CoreBind param) body localEnv -> do
      State.withState (\s -> s { env = localEnv }) $ do
        addBinding param argCore
        interp body
    a -> return $ VUtf16 (concat ["<Tried to call ", pack (showValue f), " as a function at ", pack (show app)])

addRecField recSpan fields (label, core) = do
  let valueLabel = case label of
                     CLSym labelName -> VLSym labelName
                     CLInt labelName -> VLInt labelName
  State.mapState (transform valueLabel) (interp core)
  where transform valueLabel (value, state) = (Map.insert valueLabel value fields, state)

convertValsToLabels soFar [] = return $ VLens $ reverse soFar
convertValsToLabels soFar ((CSym label _):as) = convertValsToLabels (VLSym label:soFar) as
convertValsToLabels soFar (a:as) = do
  val <- interp a
  case val of
    VInt   label -> convertValsToLabels (VLInt label:soFar) as
    VUtf16 label -> convertValsToLabels (VLSym label:soFar) as
    a            -> error $ "Invalid field: " ++ show a -- TODO: Use in-interpreter error handling

interpWithBindings span body ((CoreBind name, core):rest) = do
  addBinding name core
  interpWithBindings span body rest
interpWithBindings span body [] = interp body

interpGetLens originalArg arg _ [] = return arg
interpGetLens originalArg r@(VRec fields) soFar (a:as) = do
  case Map.lookup a fields of
    Just v  -> interpGetLens originalArg v soFar as
    Nothing -> if r /= originalArg
                 then error $ "Field " ++ showLabel a ++ " is not present at " ++ showValue (VLens $ reverse soFar) ++ " on " ++ showValue r ++ " in " ++ showValue originalArg -- TODO: Use in-interpreter error handling
                 else error $ "Field " ++ showLabel a ++ " is not present on " ++ showValue  r -- TODO: Use in-interpreter error handling
interpGetLens originalArg (VList fields) soFar ((VLInt label):as) =
  case drop (fromInteger label) fields of
    []    -> error $ "Tried to access the  " ++ show label ++ "th element of a list with only " ++ show (length fields) ++ " elements at " ++ showValue (VLens $ reverse soFar) ++ " in " ++ showValue originalArg -- TODO: Use in-interpreter error handling
    (a:_) -> interpGetLens originalArg a soFar as
interpGetLens originalArg a soFar (label:as) =
  if a /= originalArg
    then error $ "Tried to access " ++ showLabel label ++ " at " ++ showValue (VLens $ reverse soFar) ++ " on " ++ showValue a ++ " in " ++ showValue originalArg -- TODO: Use in-interpreter error handling
    else error $ "Tried to access " ++ showLabel label ++ " on " ++ showValue a -- TODO: Use in-interpreter error handling

addBinding name core = do
  value <- interp core
  State.modify' $ \state -> state { env = Map.insert name value (env state) }

showValue (VInt a)   = show a
showValue (VReal a)  = show a
showValue (VUtf16 a) = show a
showValue (VList as) = "[" ++ intercalate ", " (map showValue as) ++ "]"
showValue (VRec as)  = "{" ++ intercalate ", " (map showRecField $ Map.toList as) ++ "}"
showValue (VLens as) = intercalate "." ("#" : map showLabel as)
showValue (VClosure p body _) = showCode (CLam p body emptySpan)
showValue (VThunk   body _)   = showCode (CThunk body emptySpan)

showCode (CSym a _)      = unpack a
showCode (CInt a _)      = show a
showCode (CReal a _)     = show a
showCode (CUtf16 a _)    = unpack a
showCode (CList  a _)    = "[" ++ intercalate "," (map showCode a) ++ "]"
showCode (CRec   a _)    = "{" ++ intercalate "," (map (\(k, v) -> showCoreLabel k ++ ": " ++ showCode v) a) ++ "}"
showCode (CLet a b _)    = "let " ++ intercalate "; " (map (\(k, v) -> showBind k ++ " = " ++ showCode v) a) ++ " in " ++ showCode b
showCode (CThunk body _) = "(# " ++ showCode body ++ ")"
showCode (CLam p body _) = "(# " ++ showBind p ++ " = " ++ showCode body ++ ")"
showCode (CApp a b _)    = "(" ++ showCode a ++ " " ++ showCode b ++ ")"
showCode (CLens a _)     = intercalate "." ("#" : map showCode a)

showBind (CoreBind name) = unpack name

showRecField (label, value) = showLabel label ++ ": " ++ showValue value

showLabel (VLInt name) = show name
showLabel (VLSym name) = unpack name

showCoreLabel (CLInt label) = show label
showCoreLabel (CLSym label) = unpack label
