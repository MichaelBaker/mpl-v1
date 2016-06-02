module Mpl.Compiler where

import Mpl.AST            (AST, Core(..))
import Mpl.Desugar        (desugar)
import Mpl.TypeAnnotation (annotate)
import Mpl.Parser         (parse)
import Mpl.Interpreter    (interpret)

import Text.Earley   (Report)
import Data.Text     (Text, unpack, pack)
import Data.List     (intercalate)
import Numeric       (showFFloatAlt)

import qualified Data.Map.Strict as Map

import Prelude hiding (curry)

compile :: String -> String
compile string = case run string of
  Left  s -> s
  Right s -> s

run :: String -> Either String String
run string = do
  parsedAST <- Right $ parse (pack string)
  ast       <- handleParseFail parsedAST
  result    <- Right $ prettyPrint $ interpret $ fst $ annotate $ desugar ast
  return result

handleParseFail :: ([AST ()], Report Text Text) -> Either String (AST ())
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r

prettyPrint (CUnit  _)            = "()"
prettyPrint (CInt   _ i)          = show i
prettyPrint (CReal  _ r)          = showFFloatAlt Nothing r ""
prettyPrint (CText  _ t)          = show t
prettyPrint (CIdent _ t)          = unpack t
prettyPrint (CList  _ as)         = "[" ++ intercalate ", " (map prettyPrint as) ++ "]"
prettyPrint (CAssoc _ ps)         = "{" ++ intercalate ", " (map prettyPrintPair ps) ++ "}"
prettyPrint (CMap   _ m)          = "{" ++ intercalate ", " (map prettyPrintPair $ Map.toList m) ++ "}"
prettyPrint (CThunk _ _ b)        = "(# []" ++ prettyPrint b ++ ")"
prettyPrint (CForce _ t)          = "(" ++ prettyPrint t ++ ")"
prettyPrint (CFunc  _ _ (p, _) b) = "(# [" ++ unpack p ++ "] " ++ prettyPrint b ++ ")"
prettyPrint (CApp   _ f a)        = "(" ++ prettyPrint f ++ " " ++ prettyPrint a ++ ")"

prettyPrintPair (a, b) = prettyPrint a ++ ": " ++ prettyPrint b