module Mpl.CompilerSpec where

import Test.Hspec

import Mpl.Compiler (compile)

test_0_0 name string result = it name $ do
  compile string `shouldBe` result

spec :: Spec
spec = do
  describe "0.0" $ do
    test_0_0 "simple polymorphic lambda"
      "((@ [t] ((# [(: a t)] a) 5)) int)"
      "5"

    test_0_0 "type annotation"
      "(: int 5)"
      "5"

    test_0_0 "type operator"
      "((@ [t] ((# [(: a t)] a) 5)) (($ [s] (-> s s)) int))"
      "5"

    test_0_0 "record type"
      "(: {a int b int} 5)"
      "5"

    test_0_0 "a polymorphic record"
      "((@ [t] ((# [(: a (+ {a int b int} t))] a) 5)) {c int})"
      "5"
