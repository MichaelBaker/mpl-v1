module Mpl.CompilerSpec where

import Test.Hspec

import Mpl.Compiler (Options(..), ErrorType(..), ErrorLevel(..), errorType, compile, opts)

hasResult expectedResult (result, _, _)     = result `shouldBe` expectedResult
hasErrors expectedErrorTypes (_, _, errors) = map errorType errors `shouldBe` expectedErrorTypes

test_0_0 name string test = it name $ do
  let options = opts
  test $ compile options string

test_1_0 name string test = it name $ do
  let options = opts { typeContradictions = Fail }
  test $ compile options string

spec :: Spec
spec = do
  describe "interpreter" $ do
    test_0_0 "unit"
      "()"
      (hasResult "()")

    test_0_0 "a single int"
      "8907"
      (hasResult "8907")

    test_0_0 "a single float"
      "234.522"
      (hasResult "234.522")

    test_0_0 "a list of ints and floats"
      "  [ 123 234.5423  2 3 6 234.322] \n\n"
      (hasResult "[123 234.5423 2 3 6 234.322]")

    test_0_0 "an int -> int map"
      "{1 2 3 4}"
      (hasResult "{1 2 3 4}")

    test_0_0 "a float -> int map"
      "{1.0 23 0.3 134}"
      (hasResult "{1.0 23 0.3 134}")

    test_0_0 "a string -> float/int map"
      "{\"hello\" 1.0 \"there\" 1234}"
      (hasResult "{\"hello\" 1.0 \"there\" 1234}")

    test_0_0 "forcing a thunk"
      "((# [] 5))"
      (hasResult "5")

    test_0_0 "simple function application"
      "(((: [t] (# [a t] a)) int) 5)"
      (hasResult "5")

    -- TODO
    -- test_0_0 "simple closure"
    --   "((# [a int] ((# [_ int] a) 1)) 5)"
    --   (hasResult "5")

    -- test_0_0 "deep closure"
    --   "((# [a int] ((# [_ int] ((# [_ int] ((# [_ int] ((# [_ int] a) 1)) 2)) 3)) 4)) 5)"
    --   (hasResult "5")

    -- test_0_0 "construct pair"
    --   "((# [a int b int] [a b]) 1 2)"
    --   (hasResult "[1 2]")

    -- test_0_0 "construct list of maps"
    --   "((# [a int b int c int] [{\"a\" a} {b b} {c \"3\"}]) 1 2 3)"
    --   (hasResult "[{\"a\" 1} {2 2} {3 \"3\"}]")

    -- test_0_0 "partial application"
    --   "((# [a int b int] [a b]) 1)"
    --   (hasResult "(# [b] [a b])")

    -- test_0_0 "curried application"
    --   "(((# [a int b int] [a b]) 1) 2)"
    --   (hasResult "[1 2]")

    -- test_1_0 "parameter argument type mismatch"
    --   "((# [a int] a) \"hello\")"
    --   (hasErrors [TypeError])
