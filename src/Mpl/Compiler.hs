module Mpl.Compiler where

import Mpl.Core            (Core(..), Type)
import Mpl.ASTToCore       (astToCore)
import Mpl.Parser          (parse)
import Mpl.Interpreter     (eval)
import Mpl.TypeInterpreter (interpret)
import Mpl.TypeChecker     (TypeError(..), typeContradictions)
import Data.Text           (Text, unpack, pack)

data Result = Result {
  output   :: Text,
  warnings :: Warnings,
  errors   :: Errors
  } deriving (Show)

type Output   = String
type Warnings = [Warning]
type Errors   = [Error]

data Warning = TypeContradiction (Core Type) (Core Type) Text deriving (Show, Eq)

data Error = AmbiguousGrammar Int [Text]
           | FailedParse Text
           deriving (Show)

compile :: String -> Result
compile string = case parse (pack string) of
                   (ast:[], _) -> typeCheck $ interpret $ astToCore ast
                   (a:rest, _) -> Result {
                     output   = "",
                     warnings = [],
                     errors   = [AmbiguousGrammar (1 + length rest) $ map (pack . show) (a:rest)]
                     }
                   (_, r) -> Result {
                     output   = "",
                     warnings = [],
                     errors   = [FailedParse $ pack $ show r]
                     }

typeCheck core = Result {
  output   = pack $ prettyPrint $ eval $ core,
  warnings = map convertTypeError (typeContradictions core),
  errors   = []
  }

convertTypeError (Contradiction expected actual core) = TypeContradiction expected actual (pack $ prettyPrint core)

prettyPrint (CInt  a) = show a
prettyPrint (CReal a) = show a
prettyPrint (CText a) = show a
prettyPrint (CSym a)  = unpack a
prettyPrint (CTermApp a b) = "(" ++ prettyPrint a ++ " " ++ prettyPrint b ++ ")"
prettyPrint (CLam param tyParam body) = "(# [(: " ++ unpack param ++ " " ++ prettyPrintTy tyParam ++ ")] " ++ prettyPrint body ++ ")"
prettyPrint a = show a

prettyPrintTy CIntTy  = "int"
prettyPrintTy CRealTy = "real"
prettyPrintTy CTextTy = "text"
prettyPrintTy (CTyParam a) = unpack a
prettyPrintTy (CLamTy a b) = "(-> " ++ prettyPrintTy a ++ " " ++ prettyPrintTy b ++ ")"
prettyPrintTy a = show a
