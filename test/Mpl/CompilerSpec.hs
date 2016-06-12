module Mpl.CompilerSpec where

import Test.Hspec

import Mpl.Core     (Core(..))
import Mpl.Compiler (Options(..), Result(..), Warning(..), Error(..), compile, defaultOptions)

test_0_0 name resultField string result = it name $ do
  resultField (compile defaultOptions string) `shouldBe` result

test_1_0 name resultField string result = it name $ do
  let options = defaultOptions { typeContradictionsAreErrors = True }
  resultField (compile options string) `shouldBe` result

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

    test_0_0 "a simple type warning" warnings
      "((# [(: a int)] a) \"hello\")"
      [TypeContradictionWarning CIntTy CTextTy "((# [(: a int)] a) \"hello\")"]

    test_0_0 "ignoring type warnings" output
      "((# [(: a int)] a) \"hello\")"
      "\"hello\""

  describe "1.0" $ do
    test_1_0 "type errors prevent execution" output
      "((# [(: a int)] a) \"hello\")"
      ""

    test_1_0 "type contradictions are not warnings" warnings
      "((# [(: a int)] a) \"hello\")"
      []

    test_1_0 "type contradictions are errors" errors
      "((# [(: a int)] a) \"hello\")"
      [TypeContradictionError CIntTy CTextTy "((# [(: a int)] a) \"hello\")"]
