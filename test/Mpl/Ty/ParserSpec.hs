module Mpl.Ty.ParserSpec where

import Test.Hspec
import Mpl.Span             (emptySpan)
-- TODO: Pull ParseType into a shared module
import Mpl.Ty.Parser        (ParseType(..), parseFile, parseString)
import Text.Trifecta.Result (Result(Success, Failure))
import Helper.AST

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

spec :: Spec
spec = do
  describe "annotated expression" $ do
    testString "annotated int" "1 : Integer" Exp (ty_ann_exp (ty_int 1)  (ty_ty "Integer"))
    testString "annotated symbol" "a : Integer" Exp (ty_ann_exp (ty_sym "a")  (ty_ty "Integer"))
    testString "annotated real" "1.0 : Real" Exp (ty_ann_exp (ty_real 1) (ty_ty "Real"))
    testString "annotated lens" ".0 : Lens" Exp (ty_ann_exp (ty_lens [ty_int 0]) (ty_ty "Lens"))
    testString "annotated utf16" "\"hello\" : UTF16" Exp (ty_ann_exp (ty_utf16 "hello") (ty_ty "UTF16"))
    testString "annotated list" "[1] : List" Exp (ty_ann_exp (ty_list [ty_int 1]) (ty_ty "List"))
    testString "annotated record" "{a:1} : Record" Exp (ty_ann_exp (ty_rec [ty_field (dyn_sym "a") (ty_int 1)]) (ty_ty "Record"))
    testString "annotated let" "(let a = 1 in a) : Type" Exp (ty_ann_exp (ty_let_exp [ty_def (dyn_sym "a") (ty_int 1)] (ty_sym "a")) (ty_ty "Type"))
    testString "annotated lambda" "(# a = a) : Lambda" Exp (ty_ann_exp (ty_lam [dyn_sym "a"] (ty_sym "a")) (ty_ty "Lambda"))
    testString "annotated application" "(a b) : Integer" Exp (ty_ann_exp (ty_app (ty_sym "a") [ty_sym "b"]) (ty_ty "Integer"))
    testString "annotated lens application" "a.b : Integer" Exp (ty_ann_exp (ty_lensapp (ty_lens [ty_sym "b"]) (ty_sym "a")) (ty_ty "Integer"))
