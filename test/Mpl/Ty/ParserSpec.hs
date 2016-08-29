module Mpl.Ty.ParserSpec where

import Test.Hspec
import Mpl.Span             (emptySpan)
import Mpl.Ty.AST           (AST(..), TyAST(..))
-- TODO: Pull ParseType into a shared module
import Mpl.Ty.Parser        (ParseType(..), parseFile, parseString)
import Text.Trifecta.Result (Result(Success, Failure))

import qualified Mpl.Dyn.AST as Dyn

testFile name filename parseType expectedResult = it name $ do
  result <- parseFile parseType ("test/TestCases/Ty/Parser/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

testString name string parseType expectedResult = it name $ do
  let result = parseString parseType string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> a `shouldBe` expectedResult

-- TODO: Pull helpers into a shared module
int     a   = ADyn (Dyn.AInt   a emptySpan)
sym     a   = ADyn (Dyn.ASym   a emptySpan)
real    a   = ADyn (Dyn.AReal  a emptySpan)
utf16   a   = ADyn (Dyn.AUtf16 a emptySpan)
ty      a   = ATySym   a   emptySpan
lens    a   = ALens    a   emptySpan
list    a   = AList    a   emptySpan
rec     a   = ARec     a   emptySpan
ann_exp a b = AAnnExp  a b emptySpan
field   a b = AField   a b emptySpan
let_exp a b = ALet     a b emptySpan
def     a b = ADef     a b emptySpan
lam     a b = ALam     a b emptySpan
app     a b = AApp     a b emptySpan
lensapp a b = ALensApp a b emptySpan

spec :: Spec
spec = do
  describe "annotated expression" $ do
    testString "annotated int" "1 : Integer" Exp (ann_exp (int 1)  (ty "Integer"))
    testString "annotated symbol" "a : Integer" Exp (ann_exp (sym "a")  (ty "Integer"))
    testString "annotated real" "1.0 : Real" Exp (ann_exp (real 1) (ty "Real"))
    testString "annotated lens" ".0 : Lens" Exp (ann_exp (lens [int 0]) (ty "Lens"))
    testString "annotated utf16" "\"hello\" : UTF16" Exp (ann_exp (utf16 "hello") (ty "UTF16"))
    testString "annotated list" "[1] : List" Exp (ann_exp (list [int 1]) (ty "List"))
    testString "annotated record" "{a:1} : Record" Exp (ann_exp (rec [field (Dyn.ASym "a" emptySpan) (int 1)]) (ty "Record"))
    testString "annotated let" "(let a = 1 in a) : Type" Exp (ann_exp (let_exp [def (Dyn.ASym "a" emptySpan) (int 1)] (sym "a")) (ty "Type"))
    testString "annotated lambda" "(# a = a) : Lambda" Exp (ann_exp (lam [Dyn.ASym "a" emptySpan] (sym "a")) (ty "Lambda"))
    testString "annotated application" "(a b) : Integer" Exp (ann_exp (app (sym "a") [sym "b"]) (ty "Integer"))
    testString "annotated lens application" "a.b : Integer" Exp (ann_exp (lensapp (lens [sym "b"]) (sym "a")) (ty "Integer"))