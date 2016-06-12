module Mpl.CompilerSpec where

import Test.Hspec

import Mpl.Core     (Core(..))
import Mpl.Compiler (Result(..), Warning(..), compile)

test_0_0 name resultField string result = it name $ do
  resultField (compile string) `shouldBe` result

spec :: Spec
spec = do
  describe "0.0" $ do
    test_0_0 "simple polymorphic lambda" output
      "((@ [t] ((# [(: a t)] a) 5)) int)"
      "5"

    test_0_0 "type annotation" output
      "(: int 5)"
      "5"

    test_0_0 "type operator" output
      "((@ [t] ((# [(: a t)] a) 5)) (($ [s] (-> s s)) int))"
      "5"

    test_0_0 "record type" output
      "(: #{a int b int} 5)"
      "5"

    test_0_0 "a polymorphic record" output
      "((@ [t] ((# [(: a (| #{a int b int} t))] a) 5)) {c int})"
      "5"

    test_0_0 "a simple type error" warnings
      "((# [(: a int)] a) \"hello\")"
      [TypeContradiction CIntTy CTextTy "((# [(: a int)] a) \"hello\")"]
