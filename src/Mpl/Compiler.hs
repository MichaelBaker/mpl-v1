module Mpl.Compiler where

import Mpl.AST            (AST, Core(..), CoreType(..))
import Mpl.ASTToCore      (astToCore)
import Mpl.TypeAnnotation (annotate)
import Mpl.Parser         (parse)
import Mpl.Interpreter    (interpret)

import Text.Earley   (Report)
import Data.Text     (Text, unpack, pack)
import Data.List     (intercalate)
import Numeric       (showFFloatAlt)

import qualified Data.Map.Strict as Map

import Prelude hiding (curry)

data Options = Options {
  typeContradictions :: ErrorLevel,
  lackOfProof        :: ErrorLevel
  } deriving (Show)

data ErrorLevel = Ignore | Warn | Fail deriving (Show)

data Error = Error {
  errorType :: ErrorType,
  message   :: String
  } deriving (Show)

data ErrorType = TypeError | ParseError deriving (Show, Eq)

opts = Options {
  typeContradictions = Ignore,
  lackOfProof        = Ignore
  }

type Output   = String
type Warnings = [String]
type Errors   = [Error]

compile :: Options -> String -> (Output, Warnings, Errors)
compile options string = case run options string of
  Left  s -> ("", [], [Error ParseError s])
  Right s -> s

run :: Options -> String -> Either String (Output, Warnings, Errors)
run options string = do
  parsedAST <- Right $ parse (pack string)
  ast       <- handleParseFail parsedAST

  let (core, typeMap) = annotate $ astToCore ast
      contradictions  = Map.filter (== CUnknownTy) typeMap

  if Map.null contradictions
    then do
      result <- Right $ prettyPrint $ interpret $ core
      return (result, [], [])
    else case typeContradictions options of
           Ignore -> do
             result <- Right $ prettyPrint $ interpret $ core
             return (result, [], [])
           Warn -> do
             result <- Right $ prettyPrint $ interpret $ core
             return (result, typeErrorMessages core contradictions, [])
           Fail -> do
             return ("", [], map (Error TypeError) $ typeErrorMessages core contradictions)

handleParseFail :: ([AST ()], Report Text Text) -> Either String (AST ())
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r

prettyPrint (CUnit  _)            = "()"
prettyPrint (CInt   _ i)          = show i
prettyPrint (CReal  _ r)          = showFFloatAlt Nothing r ""
prettyPrint (CText  _ t)          = show t
prettyPrint (CIdent _ t)          = unpack t
prettyPrint (CList  _ as)         = "[" ++ intercalate " " (map prettyPrint as) ++ "]"
prettyPrint (CAssoc _ ps)         = "{" ++ intercalate " " (map prettyPrintPair ps) ++ "}"
prettyPrint (CMap   _ m)          = "{" ++ intercalate " " (map prettyPrintPair $ Map.toList m) ++ "}"
prettyPrint (CThunk _ _ b)        = "(# []" ++ prettyPrint b ++ ")"
prettyPrint (CForce _ t)          = "(" ++ prettyPrint t ++ ")"
prettyPrint (CFunc  _ _ (p, _) b) = "(# [" ++ unpack p ++ "] " ++ prettyPrint b ++ ")"
prettyPrint (CApp   _ f a)        = "(" ++ prettyPrint f ++ " " ++ prettyPrint a ++ ")"

prettyPrintPair (a, b) = prettyPrint a ++ " " ++ prettyPrint b

typeErrorMessages core contradictions = map show $ Map.toList contradictions
